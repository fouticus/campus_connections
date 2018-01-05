import sys


edges_filen = sys.argv[1]
out_filen = sys.argv[2]

#edges_filen = "CC_Edgelist.csv"
#out_filen = "CC_Edgelist_valid.csv"

# this script omits all rows where there is no edge (there are many of these)

with open(edges_filen) as f:
    with open(out_filen, "w") as f_out:
        header = f.readline()
        sn1_col = header.split(",").index("sn1")
        f_out.write(header)
        for line in f.readlines():
            if line.split(",")[sn1_col] is not "":
                f_out.write(line)
