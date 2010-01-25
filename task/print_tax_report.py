
# coding=utf-8
from __future__ import with_statement
from wget import wget
import Image, ImageSequence
import sys, os, urllib
'''class Hltb
	def self.download_tax_report docno
		tmp  = "tmp/hltb"
		fs = ["#{docno}-01.tif", "#{docno}之附件.tif"]
		url  = URI.parse('http://www.fdc.gov.tw/public/doc')
		s = false
		fs.each do |f|
			res = Net::HTTP.start(url.host, url.port) {|http|
				http.get(URI.escape("/public/doc/#{f}"))
			}
			unless res.is_a? Net::HTTPOK
				puts "Failed to download #{f}, because #{res.class.to_s}"
				next
			else
				File.open("#{tmp}/#{docno}.tif", "wb") { |file|
					file.write(res.body)
				}
				puts "Download #{f} successfully!"
				s = true
				break
			end
		end
		raise "Failed to download!" unless s
	end
	def self.print_tax_report docno
  	tmp    = "tmp/hltb"
		tmpjpg = File.join(tmp, 'jpg')
		docf = "#{tmp}/#{docno}.tif"
    imgs = Magick::ImageList.new(docf)
		[1, 4, 7, 10, 13].each do |i|
			img = File.join(tmpjpg, "#{i-1}.jpg")
      imgs[i-1].write(img)
			cmd = "rundll32 shimgvw.dll,ImageView_PrintTo d:\\fhopecc\\tmp\\hltb\\jpg\\#{i-1}.jpg sharp"
			puts cmd
			puts system(cmd)
		end
	end

end'''
def download(docno):
    fs = ["#{docno}-01.tif", "#{docno}之附件.tif"]
    url = 'http://www.fdc.gov.tw/public/doc/%s-01.tif' % docno[0:-1]
    local = os.path.join('tmp', '%s.tif' % docno[0:-1])
    wget(url, local)

def split(docno):
    local = os.path.join('tmp', '%s.tif' % docno[0:-1])
    index = 1
    im = tiffopen(local)
    index = 1
    for frame in ImageSequence.Iterator(im):
        if index in [1, 4, 7, 10, 13]:
            jpg = os.path.join('tmp', 'frame%d.jpg' % index)
            frame.save(jpg)
            cmd = r'rundll32 shimgvw.dll,ImageView_PrintTo %s sharp' 
            os.system(cmd % os.path.abspath(jpg))
        index = index + 1

def tiffopen(filename):
    cmd = r'"c:\Program Files\GnuWin32\bin\tiffcp.exe" -c none %s temp.tif' 
    print cmd
    os.system(cmd % filename)
    im = Image.open("temp.tif")
    #os.remove("temp.tif") # on windows, you have to load the data first
    return im

def usage():
    usage = u'下載並列印退稅快報\n'
    usage += u'用法: %s docno \n'
    usage += u'docno: 文號\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) == 1: raise IndexError()
        elif len(sys.argv) == 2:
            split(sys.argv[1])
    except IndexError:
        print usage()
