create_project -name mega138k_huffman -pn GW5AST-LV138FPG676AES -device_version B -force

set_option -output_base_name mega138k_huffman

add_file -type cst ../mega138k_pin_constraints.cst

source ../common.tcl
