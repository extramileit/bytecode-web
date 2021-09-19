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
  - Int value of the evaluated function
      - divide operation uses Haskell `div`
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
stack repl

# repl GHCI prompt
main

# You should see the following server message indicating successful startup 
# "Setting phasers to stun... (port 3000) (ctrl-c to quit)"

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

