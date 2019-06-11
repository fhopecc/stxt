#縣府決算
from pathlib import Path, PureWindowsPath
from aisimporter import import_gba_csv

dir = PureWindowsPath("D:\\gdrive\\縣府決算\\資訊檔案")

print(dir.as_posix())
print(dir / "cba.db")
import_gba_csv(str(dir.as_posix()),  str((dir / "cba.db").as_posix()))
