import os

tagNames_list = ["女子","山水","友情","月亮","冬天","生活","抒情","抒懷",
                  "孤獨","思念","思鄉","秋天","哲理","唐詩三百首","宮怨",
                  "送別","詠史懷古","詠物","愛情","閨怨","寫人","寫景",
                  "樂府","戰爭","離別","懷人","懷古","邊塞"]
authorNames_list = ["元結","元稹","王之渙","王昌齡","王勃","王建","王維",
                     "王翰","王灣","丘為","司空曙","白居易","朱慶餘","西鄙人",
                     "佚名","宋之問","岑參","李白","李益","李商隱","李隆基",
                     "李頎","李端","杜甫","杜牧","杜荀鶴","杜審言","沈佺期",
                     "孟郊","孟浩然","金昌緒","柳中庸","柳宗元","皇甫冉",
                     "韋莊","韋應物","祖詠","秦韜玉","馬戴","高適","崔塗",
                     "崔曙","崔顥","常建","張九齡","張旭","張泌","張祜",
                     "張喬","張籍","張繼","皎然","許渾","陳子昂","陳陶",
                     "賀知章","溫庭筠","賈島","綦毋潛","裴迪","劉方平",
                     "劉長卿","劉禹錫","劉慎虛","鄭畋","盧綸","錢起",
                     "駱賓王","戴叔倫","薛逢","韓翃","韓偓","韓愈",
                     "顧況","權德輿"]

# print(authorNames_list.index("王之渙"))



path = "./data/tang300/by_author"
dirlist = os.listdir(path)
print(dirlist)


for filename in dirlist:
	
	filename_author =filename.split(".")[0]
	filenumber = authorNames_list.index(filename_author)
	print(filename_author, filenumber + 1)
	file_rename = "{}.txt".format(filenumber + 1)
	os.rename(os.path.join(path, filename), os.path.join(path, file_rename))
