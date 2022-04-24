import yaml
from ofa.utils import download_url

resolutions=(160, 176, 192, 208, 224)
device="note10"
for image_size in resolutions:
  url = "https://hanlab.mit.edu/files/OnceForAll/tutorial/latency_table@%s/%d_lookup_table.yaml" % (device, image_size)
  download_url(url, "latency_tools/"+device, overwrite=True)

