import os
import xarray as xr
import numpy as np
# ====================== 配置区 ======================
dir_ref = '/public/share/mcc2026_final/output/'  #基准目录
dir_test = '/public/home/fujiake/fjk/stage163_gls_svml_20260813/output_full_119103481'   #个人目录
file_scs = "SCS_avg_0001.nc"
file_dong = "Dongsha60_avg_0001.nc"
# 变量阈值：(SCS阈值, Dongsha阈值)
# SCS不变；Dongsha在原缩小3量级基础上再缩小2量级（总共/1e5）
var_threshold_dict = {
    "temp": (0.00150, 0.00020),
    "salt": (0.00100, 0.00015),
    "u": (0.00040, 0.00006),
    "v": (0.00040, 0.00006),
    "zeta": (0.00050, 0.00008),
    "NO3": (0.00500, 0.00080),
    "NH4": (0.00200, 0.00030),
    "PO4": (0.00080, 0.00012),
    "diatom": (0.00200, 0.00030),
    "microzooplankton": (0.00150, 0.00020),
    "detritus": (0.00200, 0.00030),
    "oxygen": (0.05000, 0.00800),
    "TIC": (0.50000, 0.00800)
}
# ====================================================
def calc_rmse(ref_path, test_path):
    ds_ref = xr.open_dataset(ref_path, decode_times=False)
    ds_test = xr.open_dataset(test_path, decode_times=False)
    res = {}
    for var in var_threshold_dict:
        da_r = ds_ref[var]
        da_t = ds_test[var]
        err = da_t - da_r
        mse = (err ** 2).mean()
        res[var] = float(np.sqrt(mse.values))
    ds_ref.close()
    ds_test.close()
    return res

if __name__ == "__main__":
    ref_scs = os.path.join(dir_ref, file_scs)
    test_scs = os.path.join(dir_test, file_scs)
    ref_dong = os.path.join(dir_ref, file_dong)
    test_dong = os.path.join(dir_test, file_dong)

    print(f"当前工作目录: {os.getcwd()}")
    print(f"基准目录(ref): {dir_ref}")
    print(f"测试目录(test): {dir_test}\n")

    # SCS输出（带SCS阈值）
    print("=" * 80)
    print("【SCS_avg_0001.nc 所有变量全局RMSE校验】")
    scs_rmse = calc_rmse(ref_scs, test_scs)
    scs_all_ok = True
    for var, (thresh_scs, _) in var_threshold_dict.items():
        val = scs_rmse[var]
        ok = val <= thresh_scs
        status = "✅正常" if ok else "❌超限异常"
        if not ok:
            scs_all_ok = False
        print(f"{var:<20} RMSE = {val:<10.6f} 阈值≤{thresh_scs:<10.6f} {status}")

    # Dongsha输出（Dongsha阈值额外再缩小2个量级）
    print("\n" + "=" * 80)
    print("【Dongsha60_avg_0001.nc 所有变量全局RMSE校验】")
    dong_rmse = calc_rmse(ref_dong, test_dong)
    dong_all_ok = True
    for var, (_, thresh_dong) in var_threshold_dict.items():
        val = dong_rmse[var]
        ok = val <= thresh_dong
        status = "✅正常" if ok else "❌超限异常"
        if not ok:
            dong_all_ok = False
        print(f"{var:<20} RMSE = {val:<10.6f} 阈值≤{thresh_dong:<10.6f} {status}")

    # 汇总
    print("\n" + "=" * 80)
    print("【全局汇总校验结论】")
    print(f"SCS文件整体状态: {'全部正常' if scs_all_ok else '存在超限变量'}")
    print(f"Dongsha60文件整体状态: {'全部正常' if dong_all_ok else '存在超限变量'}")
    if scs_all_ok and dong_all_ok:
        print("最终判定：两组文件所有变量RMSE均在阈值范围内，优化结果无异常")
    else:
        print("最终判定：至少一组文件存在变量RMSE超限，优化结果存在异常")

