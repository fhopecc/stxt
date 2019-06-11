#教育基金決算
from pathlib import Path, PureWindowsPath
from aisimporter import import_cba_csv

dir = PureWindowsPath("D:\\hlao\\教育發展基金書審\\108單決")
print(dir.as_posix())
print(dir / "cba.db")
import_cba_csv(str(dir.as_posix()),  str((dir / "cba.db")))
