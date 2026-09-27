# 构建、运行和验证说明

远端交付路径：`/public/home/fujiake/13-MCC20268865`

## 1. 完整性检查

```bash
cd /public/home/fujiake/13-MCC20268865
sha256sum -c SHA256SUMS
bash preflight.sh
grep -n '^#undef PROFILE' ROMS/Include/globaldefs.h
grep -n -- '-fimf-use-svml=true:pow' Compilers/Linux-ifort.mk
```

若只修改了交付说明或 shell 脚本，先执行 `bash refresh_checksums.sh` 原子重建清单，再运行上述校验。`runs/` 不进入交付校验清单。

`SHA256SUMS` 不包含自身，也不包含运行后新生成的 `runs/`。

## 2. 构建

```bash
cd /public/home/fujiake/13-MCC20268865
bash build.sh
```

构建脚本使用 Intel 2021.3.0、HPC-X 2.7.4、NetCDF 4.4.1 和 HDF5 1.8.20。PGO 路径按当前源码根目录动态解析为 `$(CURDIR)/pgo`，复制或改名交付目录后也不会回指原工作树。

## 3. 正式运行

默认按本轮经验节点提交：

```bash
cd /public/home/fujiake/13-MCC20268865
sbatch submit.sh
```

当前 `submit.sh` 默认节点为历史最快有效样例 `119103481` 使用的 `j04r2n[08-11]`。无需改脚本即可覆盖节点：

```bash
sbatch --nodelist=i17r1n[08-11] submit.sh
```

必须从交付包根目录执行 `sbatch`，因为作业使用 `SLURM_SUBMIT_DIR` 定位源码和脚本；这避免了 Slurm 将脚本复制到 spool 后 `${BASH_SOURCE[0]}` 指错目录的问题。从其他目录提交时，使用：

```bash
MCC_PACKAGE_ROOT=/public/home/fujiake/13-MCC20268865 \
  sbatch --export=ALL,MCC_PACKAGE_ROOT \
  /public/home/fujiake/13-MCC20268865/submit.sh
```

脚本使用 `time` 计时，配置为 64 MPI × 2 OpenMP、8×8 tile、完整 `NTIMES=2592,12960`。每次运行写入唯一目录：

```text
runs/<SLURM_JOB_ID>/
```

其中关键证据为：

- `RUN_MANIFEST.txt`：作业、节点、并行配置及关键 SHA-256；
- `TIMING.txt`：`time` 的 real/user/sys；
- `model.log`：ROMS 标准输出，必须出现 `ROMS/TOMS: DONE`；
- `vali.log`：官方 26 项 RMSE 验证结果；
- `output/`：本次模型结果。

正式启动前脚本会运行两轮 preflight，并检查四节点分配、模块、输入软链接、关键输入文件、完整 NTIMES、8×8 tile、rank 数、验证器路径替换点和可用空间。默认最低可用空间为 `12000000 KiB`；仅当管理员确认配额足够时才可用 `MCC_MIN_FREE_KB` 覆盖。

## 4. 验证器

包内 `vali.py` 与 `/public/share/mcc2026_final/vali.py` 保持字节一致。`submit.sh` 仅在本次运行目录生成 `vali_job.py`，把官方脚本预留的个人输出路径替换为本次 `output/`，其余逻辑不变。

脚本只在以下两项均成立时返回成功：

1. 模型日志包含 `ROMS/TOMS: DONE`；
2. 官方验证器输出“两组文件所有变量 RMSE 均在阈值范围内”。

## 5. 约束与边界

- 不修改物理公式、物理网格规模、积分时间、时间步长和输出设置。
- 不关闭生态模块、不减少生态变量、不改变双向嵌套语义。
- `NtileI/NtileJ` 仅作为合法的 MPI 网格划分使用。
- `PROFILE` 为 ROMS 内部诊断开关，当前交付版已经关闭；`-prof-use` 是读取 PGO 训练数据，不是运行时 profiler。
- 跨节点的单次 wall time 只能作为节点样例，不能单独归因代码收益。
