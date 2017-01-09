# Pipelining and the microarchitecture of the PilGRIM

### Pipelining and latency
2 full cycles for the instruction cache   
3 cycles for instruction decoding, renaming and other control before an instruction actually start execution   
due to the size of the crossbar, reading registers and going through the crossbar together take 2 full cycles   
reading from the allocation heap take 2 cycles   
reading from the data cache takes 3 cycles (one extra because seperate tag check phase)   
the store unit takes one cycle to produce a new reference   


pipeline structure:

instr. kind  |  stage 1/2    |  3    |   4  |  5   |   stage 6/7     |          |
-------------|---------------|-------|------|------|-----------------|----------|----------
alu          |  ..   :  ..   |  ..   |  ..  |  ..  |   ..   :   ..   |  exec    |write back
load (a-heap)|  ..   :  ..   |  ..   |  ..  |  ..  |   ..   :   ..   |mem read 1:mem read 2|tag decode
load (cache) |instr  :instr  | pick  |decode|issue |register:crossbar|ctag check:mem read 1:mem read 2|tag decode
store        |cache 1:cache 2|expand |remame|instr |  read  :   ..   |  store   | m write 1: m write 2
call/push    |  ..   :  ..   |  ..   |  ..  |  ..  |   ..   :   ..   |          |
return       |  ..   :  ..   |  ..   |  ..  |  ..  |   ..   :   ..   | update   | m write 1: m write 2


