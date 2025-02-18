# TCL File Generated by Component Editor 19.1
# Wed Jan 22 21:01:48 CET 2020
# DO NOT MODIFY


# 
# processor "processor" v1.0
# Maciej Buszka 2020.01.22.21:01:48
# 
# 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module processor
# 
set_module_property DESCRIPTION ""
set_module_property NAME processor
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR "Maciej Buszka"
set_module_property DISPLAY_NAME processor
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


# 
# file sets
# 
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL Result
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file Result.v VERILOG PATH ../Result.v TOP_LEVEL_FILE


# 
# parameters
# 


# 
# display items
# 


# 
# connection point clock_sink
# 
add_interface clock_sink clock end
set_interface_property clock_sink clockRate 0
set_interface_property clock_sink ENABLED true
set_interface_property clock_sink EXPORT_OF ""
set_interface_property clock_sink PORT_NAME_MAP ""
set_interface_property clock_sink CMSIS_SVD_VARIABLES ""
set_interface_property clock_sink SVD_ADDRESS_GROUP ""

add_interface_port clock_sink clock clk Input 1


# 
# connection point reset_sink
# 
add_interface reset_sink reset end
set_interface_property reset_sink associatedClock clock_sink
set_interface_property reset_sink synchronousEdges DEASSERT
set_interface_property reset_sink ENABLED true
set_interface_property reset_sink EXPORT_OF ""
set_interface_property reset_sink PORT_NAME_MAP ""
set_interface_property reset_sink CMSIS_SVD_VARIABLES ""
set_interface_property reset_sink SVD_ADDRESS_GROUP ""

add_interface_port reset_sink reset reset Input 1


# 
# connection point io
# 
add_interface io avalon end
set_interface_property io addressUnits WORDS
set_interface_property io associatedClock clock_sink
set_interface_property io associatedReset reset_sink
set_interface_property io bitsPerSymbol 8
set_interface_property io burstOnBurstBoundariesOnly false
set_interface_property io burstcountUnits WORDS
set_interface_property io explicitAddressSpan 0
set_interface_property io holdTime 0
set_interface_property io linewrapBursts false
set_interface_property io maximumPendingReadTransactions 0
set_interface_property io maximumPendingWriteTransactions 0
set_interface_property io readLatency 0
set_interface_property io readWaitTime 1
set_interface_property io setupTime 0
set_interface_property io timingUnits Cycles
set_interface_property io writeWaitTime 0
set_interface_property io ENABLED true
set_interface_property io EXPORT_OF ""
set_interface_property io PORT_NAME_MAP ""
set_interface_property io CMSIS_SVD_VARIABLES ""
set_interface_property io SVD_ADDRESS_GROUP ""

add_interface_port io io_address address Input 8
add_interface_port io io_readData readdata Output 32
add_interface_port io io_writeData writedata Input 32
add_interface_port io io_write write Input 1
set_interface_assignment io embeddedsw.configuration.isFlash 0
set_interface_assignment io embeddedsw.configuration.isMemoryDevice 0
set_interface_assignment io embeddedsw.configuration.isNonVolatileStorage 0
set_interface_assignment io embeddedsw.configuration.isPrintableDevice 0

