
##########
def isInt(n):
    try: 
        int(n)
        return True
    except:
        return False

def isString(n):
    return n.startswith('\"') and n.endswith('\"')

def isBool(n):
    return n == ":true:" or n == ":false:"

def isOperator(n):
    operators = ["add","mul","sub","div","neg","pop","swap","rem","and","or","not","equal","lessThan","bind","if","let","end","call"]
    return n in operators

def isName(n):
    return (len(n) > 0) and (n[0].isalpha()) and (len([b for b in [x.isalpha() or x.isdigit() for x in n] if not b]) == 0) 
def isQuit(n):
    return n == "quit"
def isError(n):
    return n == ":error:"
def isFun(n):
    return n.startswith("fun ")
def isIOFun(n):
    return n.startswith("inOutFun ")
def isFunEnd(n):
    return n.startswith("funEnd")
def isReturn(n):
    return n == "return"

#########


def tokenize(lines):
    res = []
    funInfo = []

    for line in lines:
        print("considering: " + str(line))
        print("funInfo: " + str(funInfo))
        print("res: " + str(res))
        if line.startswith("push "):
            line = line[5:]

        outterAfterReturn = True if len(funInfo) > 0 and funInfo[-1][4] else False
        print("outterAfterReturn: " + str(outterAfterReturn))

        if isQuit(line) and not outterAfterReturn:
            res.append(("quit","quit")) if len(funInfo) == 0 else funInfo[-1][3].append(("quit","quit")) 
        elif isFun(line) and not outterAfterReturn:
            [funName,funParam] = line.split(" ")[1:]
            funType = "function"
            funInfo.append((funType,funName,funParam,[],False))
        elif isIOFun(line) and not outterAfterReturn:
            [funName,funParam] = line.split(" ")[1:]
            funType = "IOfunction"
            funInfo.append((funType,funName,funParam,[],False))
        elif isReturn(line) and not outterAfterReturn:
            funInfo[-1] = (funInfo[-1][0],funInfo[-1][1],funInfo[-1][2],funInfo[-1][3],True)
            funType = funInfo[-1][0]
            funParam = funInfo[-1][2]
            funInfo[-1][3].append(("return", "return") if funType == "function" else ("returnIO",funParam))
        elif isFunEnd(line):
            funType = funInfo[-1][0]
            funName = funInfo[-1][1]
            funParam = funInfo[-1][2]
            funCommands = funInfo[-1][3]
            if not outterAfterReturn:
                funCommands.append(("funEnd", "funEnd") if funType == "function" else ("funEndIO",funParam))
            funInfo = funInfo[:-1]
            res.append((funType,(funName,funParam,funCommands))) if len(funInfo) == 0 else funInfo[-1][3].append((funType,(funName,funParam,funCommands)))
        elif isInt(line) and not outterAfterReturn:
            res.append(("int",int(line))) if len(funInfo) == 0 else funInfo[-1][3].append(("int",int(line)))
        elif isString(line) and not outterAfterReturn:
            res.append(("string",line.rstrip('\"').lstrip('\"'))) if len(funInfo) == 0 else funInfo[-1][3].append(("string",line.rstrip('\"').lstrip('\"')))
        elif isBool(line) and not outterAfterReturn:
            res.append(("bool",True if line == ":true:" else False)) if len(funInfo) == 0 else funInfo[-1][3].append(("bool",True if line == ":true:" else False))
        elif isOperator(line) and not outterAfterReturn:
            res.append(("op",line)) if len(funInfo) == 0 else funInfo[-1][3].append(("op",line))
        elif isName(line) and not outterAfterReturn:
            res.append(("name",line)) if len(funInfo) == 0 else funInfo[-1][3].append(("name",line))
        elif isError(line) and not outterAfterReturn:
            res.append(("error","error")) if len(funInfo) == 0 else funInfo[-1][3].append(("error","error"))
        elif not outterAfterReturn:
            res.append(("error","error")) if len(funInfo) == 0 else funInfo[-1][3].append(("error","error"))

    return res

def flatten(ll):
    res = []
    for l in ll:
        for e in l:
            res.append(e)
    return res


def perform(commands,stacks,environmentStacks):

    if len(commands) == 0:
        return stacks

    print("IN PERFORM")
    print(commands[0])
    print(stacks[0])
    print(environmentStacks)

    stack = stacks[0]

    if commands[0][0] == "quit":
        return stacks
    if commands[0][0] == "int":
        upperStack = [("int" , commands[0][1])] + stack
        return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
    if commands[0][0] == "bool":
        upperStack = [("bool" , commands[0][1])] + stack
        return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
    if commands[0][0] == "string":
        upperStack = [("string" , commands[0][1])] + stack
        return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
    if commands[0][0] == "name":
        upperStack = [("name",commands[0][1])] + stack
        return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
    if commands[0][0] == "error":
        upperStack = [("error" , "error")] + stack
        return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
    if commands[0][0] == "function":
        upperStack = [("unit" , "unit" )] + stack
        funName = commands[0][1][0]
        funParam = commands[0][1][1]
        funCommands = commands[0][1][2]
        return perform(commands[1:],[upperStack] + stacks[1:], [[(funName,"function",(funParam,funCommands, flatten(environmentStacks)))] + environmentStacks[0]] + environmentStacks[1:] ) #add function binding to environment
    if commands[0][0] == "IOfunction":
        upperStack = [("unit" , "unit" )] + stack
        funName = commands[0][1][0]
        funParam = commands[0][1][1]
        funCommands = commands[0][1][2]
        return perform(commands[1:],[upperStack] + stacks[1:], [[(funName,"functionIO",(funParam,funCommands, flatten(environmentStacks)))] + environmentStacks[0]] + environmentStacks[1:] ) #add function binding to environment
    
    if commands[0][0] == "return":
        #upperStack = [stack[0]] + stacks[1]
        upperStack = [stack[0] if stack[0][0] != "name" else lookupInenvironmentStacks(environmentStacks,stack[0][1])] + stacks[1]
        return perform(commands[1:],[upperStack] + stacks[2:],environmentStacks[1:] )
    if commands[0][0] == "funEnd":
        return perform(commands[1:],stacks[1:],environmentStacks[1:] )
    if commands[0][0] == "returnIO":
        paramName = commands[0][1]
        (paramType,paramValue) = lookupInenvironmentStacks(environmentStacks,paramName)
        upperStack = [stack[0] if stack[0][0] != "name" else lookupInenvironmentStacks(environmentStacks,stack[0][1])] + stacks[1]
        argName = commands[0][2]
        upperEnvironment = [(argName,paramType,paramValue)] + environmentStacks[1] #NOT PARAM NAME BUT ACTUAL ARGUMENT NAME commands[0][2] will be the argument of the function call
        return perform(commands[1:],[upperStack] + stacks[2:], [upperEnvironment] +  environmentStacks[2:] )
    if commands[0][0] == "funEndIO":
        paramName = commands[0][1]
        (paramType,paramValue) = lookupInenvironmentStacks(environmentStacks,paramName)
        argName = commands[0][2]
        upperEnvironment = [(argName,paramType,paramValue)] + environmentStacks[1] #NOT PARAM NAME BUT ACTUAL ARGUMENT NAME commands[0][2] will be the argument of the function call
        return perform(commands[1:],stacks[1:], [upperEnvironment] +  environmentStacks[2:] )

    if commands[0][0] == "op":
        op = commands[0][1]
        ###NO REQUIREMENTS OPS
        if (op == "let"):
            upperStack = []
            return perform(commands[1:],[upperStack] + stacks ,[[]] + environmentStacks)
        if (op == "end"):
            upperStack = [stacks[0][0]] + stacks[1]
            newStack = [upperStack] + stacks[2:]
            newEnvironment = environmentStacks[1:]
            return perform(commands[1:],newStack,newEnvironment)


        ###UNARY OPS
        if (len(stack) > 0):
            typ1 = stack[0][0]
            val1 = stack[0][1]
            if typ1 == "name":
                (typ1,val1) = lookupInenvironmentStacks(environmentStacks,val1)

            if (op == "neg") and (typ1 == "int"):
                upperStack = [("int" , val1*-1)] + stack[1:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "not") and (typ1 == "bool"):
                upperStack = [("bool" , not val1)] + stack[1:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "pop"):
                upperStack = stack[1:] + stacks[1:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)

        ###BINARY OPS
        if (len(stack) > 1):
            typ1 = stack[0][0]
            val1 = stack[0][1]
            typ2 = stack[1][0]
            val2 = stack[1][1]

            if typ1 == "name":
                print("looking up 1 " + val1)
                (typ1,val1) = lookupInenvironmentStacks(environmentStacks,val1)
                print("found" +  str(val1) + ":" + str(typ1))
            if typ2 == "name":
                print("looking up 2 " + val2)
                (typ2,val2) = lookupInenvironmentStacks(environmentStacks,val2)
                print("found" +  str(val2) + ":" + str(typ2))

            if (op == "add") and (typ1 == "int") and (typ2 == "int"):
                upperStack = [("int",val2+val1)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "mul") and (typ1 == "int") and (typ2 == "int"):
                upperStack = [("int",val2*val1)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "sub") and (typ1 == "int") and (typ2 == "int"):
                upperStack = [("int",val2-val1)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "div") and (typ1 == "int") and (typ2 == "int") and (val1!=0):
                upperStack = [("int",int(val2/val1))] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "rem") and (typ1 == "int") and (typ2 == "int") and (val1!=0):
                upperStack = [("int",val2%val1)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "equal") and (typ1 == "int") and (typ2 == "int"):
                upperStack = [("bool",val1 == val2)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "lessThan") and (typ1 == "int") and (typ2 == "int"):
                upperStack = [("bool",val2 < val1)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "swap"):
                upperStack = [(stack[1][0],stack[1][1]),(stack[0][0],stack[0][1])] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "and") and (typ1 == "bool") and (typ2 == "bool"):
                upperStack = [("bool",val1 and val2)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
            if (op == "or") and (typ1 == "bool") and (typ2 == "bool"):
                upperStack = [("bool",val1 or val2)] + stack[2:]
                return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)

            if (op == "call") and (typ1 == "function") and (val2 != "error"):
                (arg,funCommands,closureEnvironment) = val1
                (typ2,val2) = (typ2,val2) if typ2 != "name" else lookupInenvironmentStacks(environmentStacks,val2) #lookup argument if name
                if typ2 == "name":
                   return perform(commands[1:],[[("error","error")] + stack] + stacks[1:],environmentStacks)
                newStack = [[]] + [stacks[0][2:]] + stacks[1:]
                closureEnvironment = [(arg,typ2,val2)] + closureEnvironment # bind formal parameter
                newEnvironment = [closureEnvironment] + environmentStacks
                return perform( funCommands + commands[1:],newStack,newEnvironment)
            if (op == "call") and (typ1 == "functionIO") and (val2 != "error"):
                (param,funCommands,closureEnvironment) = val1
                if stack[1][0] != "name":
                    return perform(commands[1:],[[("error","error")] + stack] + stacks[1:],environmentStacks)
                (typ2,val2) = (typ2,val2) if typ2 != "name" else lookupInenvironmentStacks(environmentStacks,val2) #lookup argument if name
                if typ2 == "name":
                   return perform(commands[1:],[[("error","error")] + stack] + stacks[1:],environmentStacks)
                newStack = [[]] + [stacks[0][2:]] + stacks[1:]
                closureEnvironment = [(param,typ2,val2)] + closureEnvironment # bind formal parameter
                newEnvironment = [closureEnvironment] + environmentStacks
                funCommands[-1] = (funCommands[-1][0],funCommands[-1][1],stack[1][1]) #assume the input to the IOfunction is a name and make that the second field in the last command
                return perform( funCommands + commands[1:],newStack,newEnvironment)

            #restore for bind i.e. don't lookup name
            typ2 = stack[1][0]
            val2 = stack[1][1]
            if (op == "bind") and (typ1 == "int" or typ1 == "bool" or typ1 == "string" or typ1 == "unit") and typ2 == "name":
                upperStack = [("unit","unit")] + stack[2:]
                upperEnvironment = [(val2,typ1,val1)] + environmentStacks[0]
                print(val1)
                print(typ1)
                print(val2)
                print(typ2)
                print(("in bind",environmentStacks[0]))
                return perform(commands[1:],[upperStack] + stacks[1:], [upperEnvironment] + environmentStacks[1:] ) #push to top list environmentStacks [[(name,type,value)]]
                                                


        ###TERNARY OPS
        if (len(stack) > 2):
            typ1 = stack[0][0]
            val1 = stack[0][1]
            typ2 = stack[1][0]
            val2 = stack[1][1]
            typ3 = stack[2][0]
            val3 = stack[2][1]

            if typ1 == "name":
                (typ1,val1) = lookupInenvironmentStacks(environmentStacks,val1)
            if typ2 == "name":
                (typ2,val2) = lookupInenvironmentStacks(environmentStacks,val2)
            if typ3 == "name":
                (typ3,val3) = lookupInenvironmentStacks(environmentStacks,val3)

            if (op == "if") and (typ3 == "bool"):
                if (val3):
                    upperStack = [(typ1,val1)] + stack[3:]
                    return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)
                else:
                    upperStack = [(typ2,val2)] + stack[3:]
                    return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)


        ###ERROR
        upperStack = [("error","error")] + stack
        return perform(commands[1:],[upperStack] + stacks[1:],environmentStacks)


def lookupInenvironmentStacks(environmentStacks,name):
    for s in environmentStacks:
        for e in s:
            if name == e[0]:
                return (e[1],e[2]) # (type,value)
    return ("name",name)

def printStack(stack,outfile):
    res = ""
    for s in stack:
        typ = s[0]
        val = s[1]
        if (typ == "error"):
            res = res + ":error:\n"
        elif (typ == "bool"):
            res = res + (":true:\n" if val else ":false:\n")
        elif (typ == "unit"):
            res = res + ":unit:" + "\n"
        else:
            res = res + str(val) + "\n" 
    f = open(outfile,"w")
    f.write(res)
    f.close()


def interpreter(infile,outfile):
    f = open(infile)
    raw = f.readlines()
    f.close()
    lines = [x.strip() for x in raw]
    print(lines)
    tokens = tokenize(lines)
    #print(tokens)
    for t in tokens:
        print(t)


    #return
    #RETURN TO TEST TOKENIZE

    stacks = [[]]
    environmentStacks = [[]] # should be list of lists where let is a new list and end is pop

    res = perform(tokens,stacks,environmentStacks)
    print("res is " + str(res))
    printStack(res[0],outfile)

#Testing phase 1

for n in range(1,7):
   interpreter("tests/phase1/input_" + str(n) + ".txt","tests/phase1/py/output_" + str(n) + ".txt")

#Testing phase 2

for n in range(1,11):
   interpreter("tests/phase2/input_" + str(n) + ".txt","tests/phase2/py/output_" + str(n) + ".txt")

#Testing phase 3

for n in range(1,22):
   interpreter("tests/phase3/input_" + str(n) + ".txt","tests/phase3/py/output_" + str(n) + ".txt")




















