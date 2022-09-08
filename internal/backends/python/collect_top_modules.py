import json
from binary_heap import MinHeap

class PackageDownloadInfo:
    def __init__(self, name, downloads):
        self.name = name
        self.downloads = downloads

    def __eq__(self, other):
        return self.downloads == other.downloads
    
    def __gt__(self, other):
        return self.downloads > other.downloads
    
    def __lt__(self, other):
        return self.downloads < other.downloads
    
    def __repr__(self):
        return "%s(%d)" % (self.name, self.downloads)

heap = MinHeap()
bq_file = open("download_counts.json")
stats = json.load(bq_file)
bq_file.close
for pkg, downloads in stats.items():
    info = PackageDownloadInfo(pkg, downloads)
    heap.add_element(info)
    while heap.length() > 10000:
        heap.extract_root()

bq_file.close()

pkgs = list([pkg.name for pkg in heap.elements()])
pkgs.sort(key=lambda pkg: stats[pkg], reverse=True)

top_10k = open("top_10k.json", "w")
json.dump(pkgs, top_10k)
top_10k.close()
print("Wrote top_10k.json")

