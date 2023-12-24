def test(val):
    if(val != 3):
        return test(3)
    print("siema")

test(5)