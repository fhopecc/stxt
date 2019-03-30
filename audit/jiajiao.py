#文化局決算
from pathlib import Path, PureWindowsPath
from aisimporter import import_cba_csv

dir = PureWindowsPath("C:\\Users\\fhopecc\\Google 雲端硬碟\\家教中心決算\\dataset")
print(dir.as_posix())
print(dir / "cba.db")
import_cba_csv(str(dir.as_posix()),  str((dir / "cba.db").as_posix()))
