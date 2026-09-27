# MCC2026 ROMS-CoSiNE 交付包

## 最佳结果

| Job | 节点 | real | 完整输入 | 官方验证 |
|---|---|---:|---|---|
| `119103481` | `j04r2n[08-11]` | `1622.705s` | `NTIMES=2592,12960` | `26/26 PASS` |

## 核心配置

- 4 节点、64 MPI rank、每 rank 2 OpenMP 线程。
- 两个网格均为 `NtileI=8, NtileJ=8`。
- 正式输入 `NTIMES=2592,12960`。
- ROMS 内部 `PROFILE` 已关闭：`ROMS/Include/globaldefs.h` 为 `#undef PROFILE`。
- 全局精确浮点、AVX2、no-FMA、IPO、PGO；`gls_corstep.o` 单独使用 `-fp-model source -fimf-use-svml=true:pow`。
- `Inputfiles` 是只读共享输入 `/public/home/fujiake/Inputfiles` 的符号链接；包内不复制 NetCDF 输入或输出。

## 入口

- 静态路径预检：`bash preflight.sh`
- 构建：`bash build.sh`
- 提交：`sbatch submit.sh`
- 完整说明：`RUN_INSTRUCTIONS.md`
- 文件清单：`PACKAGE_MANIFEST.txt`
- 文件校验：`SHA256SUMS`
- 修改交付脚本后刷新清单：`bash refresh_checksums.sh`

必须先 `cd` 到交付包根目录再执行 `sbatch submit.sh`。作业通过 `SLURM_SUBMIT_DIR` 找到交付包；如必须从其他目录提交，应显式导出 `MCC_PACKAGE_ROOT=/public/home/fujiake/13-MCC20268865`

## 关于本仓库

本仓库整理自 MCC2026 决赛交付目录 `~/13-MCC20268865`，保留 ROMS-CoSiNE 源码、编译配置、提交脚本和验证器，便于阅读、归档及在比赛环境中复现。

上面的最佳结果来自原交付说明中的历史记录。本地整理没有重新编译或运行比赛任务；运行日志和模型输出未随源码复制，耗时也不代表其他机器上的性能。

## 目录结构

| 路径 | 内容 |
|---|---|
| `ROMS/` | 模型源码、头文件和运行参数；本次主输入文件位于 `ROMS/External/` |
| `Master/`、`User/`、`Projects/` | 程序入口、用户配置及项目文件 |
| `Compilers/`、`makefile` | 编译规则；本次 Intel 配置位于 `Compilers/Linux-ifort.mk` |
| `Lib/`、`Atmosphere/`、`Waves/` | 随交付包保留的库及相关模块 |
| `Data/`、`test/`、`matlab/`、`plot/` | 随包保留的数据模板、测试及分析绘图文件 |
| `Reference/` | 参考文献 |
| `pgo/pgopti.dpi` | PGO（依据已有运行数据优化编译）的训练数据库，构建脚本要求保留 |
| `build.sh`、`preflight.sh` | 构建及环境、输入文件检查 |
| `submit.sh`、`coll_rules.conf` | Slurm 作业提交及 MPI 集合通信配置 |
| `vali.py` | 随交付包保留的官方验证器 |

## 本地副本与 Git 上传

本地副本已添加 `.gitignore`，忽略运行目录 `runs/`、输入链接 `Inputfiles`、`oceanM` 可执行文件、常见编译产物、日志以及编辑器临时文件。`pgo/pgopti.dpi` 保留在上传范围内。

此次复制排除了约 8.5 GB 的 `runs/`，未复制外部输入数据，也未在 Windows 上创建 Linux 输入链接。本地仍保留复制得到的 `oceanM`，但正常执行 `git add` 时会将它忽略。`.gitignore` 仅控制 Git 收录范围，手动打包或网页上传整个目录时需要另行排除这些文件。

为兼容 Windows，两个中文参考文献文件名已改为：

- `Reference/2009-Zhou-Changjiang-diluted-water-variation.pdf`
- `Reference/2009-Zhou-Bohai-summer-thermocline-cold-water.pdf`

参考文献内容保持不变。原远端目录没有 `.git` 历史；首次上传时，可在当前目录执行以下命令，并在提交前查看暂存内容：

```bash
git init
git add .
git status --short
git diff --cached --stat
```

确认文件范围后，再提交并绑定自己的远程仓库地址：

```bash
git commit -m "Import MCC2026 final source code"
git branch -M main
git remote add origin <你的仓库地址>
git push -u origin main
```

## 在比赛服务器上准备运行

脚本面向 Linux 集群，依赖环境模块、Intel 编译器、MPI、NetCDF、HDF5 和 Slurm 作业调度系统。Windows 本地副本用于整理和上传；构建、提交及验证需在具备相应环境的服务器上进行。

`build.sh` 加载 Intel 2021.3.0、HPC-X 2.7.4、NetCDF 4.4.1 和 HDF5 1.8.20，并使用 `/opt/rh/devtoolset-7/root/usr/bin/gmake`。`submit.sh` 的运行环境加载 Intel 2017.5.239，验证阶段依赖比赛共享目录下的 Conda `vali` 环境。迁移到其他集群时，需要检查这些模块名、路径、分区及 CPU 核心绑定设置。

在仓库根目录恢复输入链接。下面的路径适用于原比赛账号；其他账号应替换为实际输入目录，并确认其中包含完整的 `SCS/` 和 `Dongsha60/` 数据：

```bash
ln -s /public/home/fujiake/Inputfiles Inputfiles
```

**首次从 Git 克隆后的构建限制：** 当前 `build.sh` 在编译前调用 `preflight.sh`，后者要求 `oceanM` 已存在且可执行。由于 `oceanM` 被 Git 忽略，新克隆目录直接执行构建会在此处停止。使用现有脚本时，需要先从原交付包另行取得 `oceanM` 并恢复执行权限；如需支持完全从源码开始构建，应先调整预检脚本中对已有可执行文件的检查。

当输入数据和原交付可执行文件准备齐全后，可按以下顺序操作：

```bash
chmod +x oceanM
bash preflight.sh
bash build.sh
sbatch submit.sh
```

默认提交到 `kshcexclu06` 分区，使用 4 个节点 `j04r2n[08-11]`。这些是原比赛环境配置，提交前需确认资源仍可用。节点覆盖方式及从其他目录提交的方法见 [RUN_INSTRUCTIONS.md](RUN_INSTRUCTIONS.md)。

## 运行结果与验证

每次作业默认写入独立目录 `runs/<SLURM_JOB_ID>/`，主要文件如下：

| 文件 | 用途 |
|---|---|
| `RUN_MANIFEST.txt` | 记录节点、并行配置及关键文件校验值 |
| `TIMING.txt` | 记录模型运行的 real、user、sys 时间 |
| `model.log` | 模型日志，完成时应包含 `ROMS/TOMS: DONE` |
| `vali.log` | 验证结果，应包含全部变量 RMSE 通过的最终判定 |
| `output/` | 本次模型输出 |

提交脚本仅在模型完成检查和验证器最终判定均通过后输出 `RUN_AND_VALIDATION_PASS`。复现实验时应同时保存计时、验证结果和节点配置，便于比较不同运行。

## 校验清单说明

当前 `PACKAGE_MANIFEST.txt` 和 `SHA256SUMS` 保留自原交付包。README 续写、参考文献改名及文件范围变化后，它们已不能作为当前 Git 副本的完整性校验依据；直接执行 `sha256sum -c SHA256SUMS` 会出现不匹配或文件缺失。

`refresh_checksums.sh` 用于重新生成交付目录清单，但它目前只排除 `runs/` 和清单自身等文件，**不会按 `.gitignore` 过滤，也不会排除 `.git/`**。如需生成新的交付校验清单，应在准备好的独立交付目录中执行，确保该目录不含 `.git/` 和无关本地文件，再运行：

```bash
bash refresh_checksums.sh
sha256sum -c SHA256SUMS
```

## 源码与使用约束

复现比赛结果时保持物理公式、网格规模、积分时间、时间步长、生态模块和输出设置不变。具体约束见 [RUN_INSTRUCTIONS.md](RUN_INSTRUCTIONS.md)。ROMS 许可说明见 [ROMS/License_ROMS.txt](ROMS/License_ROMS.txt)，其他随附代码应同时遵循各自的许可声明。
