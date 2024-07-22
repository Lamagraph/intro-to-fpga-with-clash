create_project -name primer25k_huffman -pn GW5A-LV25MG121NES -device_version A -force ; # <1>
set_option -output_base_name primer25k_huffman ; # <2>
add_file -type cst ../primer25k_pin_constraints.cst ; # <3>
source ../common.tcl ; # <4>
