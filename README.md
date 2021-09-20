# bytecode-web

- Runs a web server that handles HTTP POST requests containing "byte code" according to a stack architecture
    
- The request body:
    - must start with blocks of variable assignments followed by blocks of operation command instructions
    - Sample Input
      ```
      LOAD_VAL 2
      WRITE_VAR 'x'

      LOAD_VAL 2
      WRITE_VAR 'y'

      LOAD_VAL 5
      WRITE_VAR 'z'

      READ_VAR 'x'
      READ_VAR 'z'
      ADD

      READ_VAR 'y'
      MULTIPLY

      LOAD_VAL 3
      DIVIDE

      RETURN_VALUE
      ```
- The response provides:
  - interpretation of the byte code in the form of a function
  - Float value of the evaluated function
  - sample response
    ```
    function f() {
        x = 2
        y = 2
        z = 5
        return (x + z) * y / 3
    }
    f() = 4.6666665
    ```
### Build Project and Run on localhost:3000
```
# in a terminal
stack build
stack exec bytecode-web-exe

# You should see the following server message indicating successful startup 
# "Setting phasers to stun... (port 3000) (ctrl-c to quit)"
```

### Build and run using repl
```
# in a terminal
stack repl

# at repl GHCI prompt
main
```

### Run tests

```
# in a terminal
stack test
```

### Send sample HTTP POST request using curl
```
# in a new terminal
curl --location --request POST 'http://localhost:3000' \
--header 'Content-Type: text/plain' \
--data-raw 'LOAD_VAL 1
WRITE_VAR '\''x'\''
LOAD_VAL 2
WRITE_VAR '\''y'\''
READ_VAR '\''x'\''
LOAD_VAL 1
ADD
READ_VAR '\''y'\''
MULTIPLY
RETURN_VALUE'
```

### Explanation of the code

#### Modules
1. Main (app dir)- starts an HTTP listener process on localhost, port 3000
2. DataTypes (src dir) - defines the data structures needed to store the information received in the HTTP request
3. Parser (src dir) - does the heavy lifting of parsing the request to construct a ByteCode data structure
4. RunByteCode (src dir) - unwraps the ByteCode data structure to build a response that includes
the interpeted function body output and the evaluation of the function

#### Design Considerations
- The Main module starts the HTTP listener and on receiving a POST request, it delegates to a handlePostRequest function which does the following
    - pre-processing to remove blank lines
    - call Parser module - toByteCode to construct the ByteCode type
    - call RunByteCode module - runByteCode to build the response
- The design of Parser module is based on some assumptions of how the input will look.  
    - The approach I took was to split the request lines into a pair of sublists:
        - The 1st sublist contains the lines having to do with variable assignment and the 2nd sublist contains the lines that process math operations.
    - In order to split the lines, the program assumes that the variable assignment lines are at the top, so
      it finds the last WRITE_VAR line and splits the body of lines based on the index of that line + 1. 
    - For processing the 1st sublist of lines, the program assumes that every variable assignment is two lines (i.e. LOAD_VAL and WRITE_VAR). 
    - The findVariableAssignPairs uses a List Monad "Do block" to process the lines and build pairs
      of variable name and corresponding Int value. Once it has the variable name/value pairs, 
      then it is easy to build an Ordered Map data structure to store the pairs for fast retrieval.
      I chose an ordered map because the function output should show the variables in the same order as they
      were received in the request body.
    - For processing the 2nd sublist of lines, the program assumes the every math operation will end with 
      a Math operator "command" (e.g. ADD, MULTIPLY), so it splits this list into MathOperationGrouping blocks.
      Once the operation lines are grouped, it constructs OperationCommand types.
        - OperationCommand types must be one of three kinds:
            1) a binomial which holds a math operation and two terms,
            2) partial which holds a math operation and one term, and 
            3) ReturnVal which will be the final operation to return
      Only the first math operation grouping should be binomial.
    - The result of toByteCode function will construct the ByteCode type defined as follows
      ```
        data ByteCode = ByteCode {
            variableMap :: OMap T.Text Int
          , operationCommands :: [OperationCommand]
        }

        data MathOp = ADD | SUBTRACT | MULTIPLY | DIVIDE | RETURN_VALUE
            deriving (Show, Eq)

        data OperationCommand = Binomial MathOp T.Text T.Text
                      | Partial MathOp T.Text
                      | ReturnVal
        deriving Show

      ```  
  
- The design of the RunByteCode module centers on the runByteCode function, which is doing two main things--
    1) show the request data in the form of function output 
    2) calculating the function output 
         
    - The code to show request in the form of a function output
  
        ```
        makeFunction :: ByteCode -> T.Text
        makeFunction bc = "function f() {\n"
        <> makeVariableAssignments (MO.assocs (variableMap bc))
        <> "\n"
        <> "\t" <> makeReturnExpression bc
        <> "\n}"
       ```
    - The makeReturnExpression uses foldl to apply the commands and create a Text output of the return expression.
      A tricky part was figuring out when parens need to be added to the accumulated value of the foldl function.
      The program checks if the accumulated value has a trailing add or subtract operation and if the next
      operation is a multiply or divide operation.  If so, then it wraps the accumulated value in parens. 
    - The code to calculate the function output as a Float value also uses a foldl function
      I thought about using a Stack data structure, which would pop the term(s) off the stack, 
      then apply the math operation and push the result on the stack, then repeat until the stack holds the final value.
      However, I could accomplish the same thing with the foldl, so there was no need to populate another data structure.

#### Performance Notes
- In terms of performance, the program avoids the need to iterate over the lines of the request
  more than once.  For the most part, I was able to use Text types to store the string data.
  Processing with Text will be more efficient than String data type as the compiler can avoid multiple allocations
  of Text when there is some mapping of text data. https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings