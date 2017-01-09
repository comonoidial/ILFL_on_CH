# The architecture and instruction set of the PilGRIM

### Choosing hardware component widths and constraints
the datapath is 64 bits wide and all data in memory and registers is organized and addressed in 64 bits words   
nodes are limited to 8 words (1 tag word and 7 values)   
heap is divided in 4 word elements   
most of the nodes on the heap fit in 4 words, more adds a lot to hardware cost and only gives a small performance benefit   
reading or writing nodes takes extra cycles if the node is more than 4 words   
only the top 4 entries of the node stack can be read directly   
reference queue is limited to last 16 reference produced   
primitive queue is limited to last 16 primitive value produced   
for the continuation stack, 2 words wide is enough for most function applications   


