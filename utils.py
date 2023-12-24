def check_bit(x,idx,bit_len):
    idx = bit_len - idx - 1
    return (x >> idx) & 1

def check_lst_bit(x):
    return (x >> 0) & 1

def reach_number(x,r):
    if(x == 0):return []

    operations = [f"INC {r}"]
    bit_len = x.bit_length()
    num = 1
    idx = 1
    while(num != x):
        num = num * 2
        operations.append(f"SHL {r}")
        if(check_lst_bit(num) != check_bit(x,idx,bit_len)):
            num = num + 1
            operations.append(f"INC {r}")
        idx += 1
    
    return operations





