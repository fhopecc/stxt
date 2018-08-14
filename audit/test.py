from aisimporter import parse_table_name
import unittest

class TestMethods(unittest.TestCase):

    def test_parse_table_name(self):
        # \w+ - \d+\.(\w+) - \d+\.csv
        self.assertEqual(parse_table_name("花蓮縣議會 - 13.受款人異常關聯 - 1070726.csv"), '受款人異常關聯')
        # \w+-\d+-(\w+).csv
        self.assertEqual(parse_table_name("資料匯出-20180725082947-發票檔.csv"), '發票檔')


if __name__ == '__main__':
    unittest.main()
