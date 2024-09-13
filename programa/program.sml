(* Función para limpiar los cambios de línea *)
fun limpiar_cambios_linea str = 
    if (str = "") then ""
    else if (String.substring(str, 0, 1) = "\n") 
        then limpiar_cambios_linea(String.substring(str, 1, (size str) - 1))
    else
        String.substring(str, 0, 1) ^ limpiar_cambios_linea(String.substring(str, 1, (size str) - 1));


fun printLine msg = print (msg ^ "\n");

fun writeFileA filename content = 
    let val fd = TextIO.openAppend filename
        val _ = TextIO.output (fd, content) handle e =>(TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in
        ()
    end;

(* Función auxiliar para dividir una cadena en una lista de cadenas usando un delimitador (en este caso, ",") *)
fun splitLine line =
    String.tokens (fn c => c = #",") line;

(* Función para leer el archivo CSV y devolver una lista de transacciones *)
fun leerCsv path =
    let
        val archivo = TextIO.openIn path
        val contenido = TextIO.inputAll archivo
        val _ = TextIO.closeIn archivo
        val lineas = String.tokens (fn c => c = #"\n") contenido
    in
        map splitLine (List.tl lineas)
    end;


fun addRegister () = 
    let
        val _ = printLine "Ingrese la ruta del archivo (.csv):";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val path = limpiar_cambios_linea temp;
        
        val _ = printLine "Ingese la cuenta:";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val account = limpiar_cambios_linea temp;

        val currentTime = Time.now ();
        val date = Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal currentTime);

        val _ = printLine "Ingese el tipo de transaccion:";
        val _ = printLine "1. Deposito";
        val _ = printLine "2. Retiro";
        val _ = printLine "3. Transferencia";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val ttTempr = limpiar_cambios_linea temp;

        val _ = printLine "Ingese la catidad de la transferencia (0.00):";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val amount = limpiar_cambios_linea temp;

        val (destinationAccount, transactionType) =
        case ttTempr of
             "1" => ("", "deposito")
            | "2" => ("", "retiro")
            | "3" => (let
                        val _ = printLine "Ingese la cuenta destino:";
                        val temp = valOf (TextIO.inputLine TextIO.stdIn);
                        val destinationAccount = limpiar_cambios_linea temp;
                        in
                        (destinationAccount, "transferencia")
                        end)
            | _ => (printLine "Opcion invalida"; addRegister (); ("", ""))


        val line = "\n" ^ account ^ "," ^ date ^ "," ^ transactionType ^ "," ^ amount ^ "," ^ destinationAccount ^ "\n";
        val temp = writeFileA path line;

        val _ = printLine "Registro añadido exitosamente...";
        val _ = printLine "Volcien"
    in 
        ()
    end;

fun cleanIndex () =
    let
        val _ = printLine "Ingrese la ruta del archivo (.csv):";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val path = limpiar_cambios_linea temp;
        
        (*Reescribe le archivo*)
        val outputFile = TextIO.openOut path
        val _ = TextIO.output (outputFile, "account_id,transaction_date,transaction_type,amount,destination_account_id")
        val _ = TextIO.closeOut outputFile
        
        val _ = printLine "Se esta eliminando el indice...";
        val _ = printLine "Se eliminó el indice correcamente."
    in 
        ()
    end;

fun showTop () = (print "Mostrando Top\n");

fun suspiciousReport () = (print "Reporte sospechoso\n");

fun transactionsPerAccount () = 
    let
        val _ = printLine "Ingrese la ruta del archivo (.csv):";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val path = limpiar_cambios_linea temp;
        
        val _ = printLine "Ingese la cuenta:";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val account = limpiar_cambios_linea temp;

        val transactions = leerCsv path;
    in
        ()
    end;



fun countTransactions transactions = 
    let
        val deposito = length (List.filter (fn x => List.nth(x, 2) = "deposito") transactions)
        val retiro = length (List.filter (fn x => List.nth(x, 2) = "retiro") transactions)
        val transferencia = length (List.filter (fn x => List.nth(x, 2) = "trasferencia") transactions)
    in
        (deposito, retiro, transferencia)
    end;

fun transactionsQuantity () = 
    let
        val _ = printLine "Ingrese la ruta del archivo (.csv):";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val path = limpiar_cambios_linea temp;

        val _ = printLine "Ingese el tipo de transaccion:";
        val _ = printLine "1. Deposito";
        val _ = printLine "2. Retiro";
        val _ = printLine "3. Transferencia";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val ttTempr = limpiar_cambios_linea temp;

        val transactions = leerCsv path;
        val (numDepositos, numRetiros, numTransferencias) = countTransactions transactions;

        val msg =
        case ttTempr of
             "1" => ("La cantidad de depositos son: " ^ Int.toString numDepositos)
            | "2" => ("La cantidad de retiros son: " ^ Int.toString numRetiros)
            | "3" => ("La cantidad de transaferencias son: " ^ Int.toString numTransferencias)
            | _ => (printLine "Opcion invalida"; transactionsQuantity (); (""))
        
        val _ = printLine msg;
    in
        ()
    end;

(* Función para convertir el monto a real y manejar el caso de NONE *)
fun amountStrInt transaccion = 
    case Real.fromString(List.nth(transaccion, 3)) of
         SOME(monto) => monto
       | NONE => 0.0;  

fun greaterAmountTransaction [] = []
  | greaterAmountTransaction [x] = x  
  | greaterAmountTransaction (x::xs) =
    let
        val tailMax = greaterAmountTransaction xs  
    in
        if amountStrInt(x) > amountStrInt(tailMax) then x else tailMax
    end;

fun lesserAmountTransaction [] = []
  | lesserAmountTransaction [x] = x  
  | lesserAmountTransaction (x::xs) =
    let
        val tailMax = lesserAmountTransaction xs  
    in
        if amountStrInt(x) < amountStrInt(tailMax) then x else tailMax
    end;


fun summary () =  
    let 
        val _ = printLine "Ingrese la ruta del archivo (.csv):";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val path = limpiar_cambios_linea temp;

        (*Reads the ransaction in the csv file*)
        val transactions = leerCsv path;

        (* Contar las transacciones por tipo *)
        val (numDepositos, numRetiros, numTransferencias) = countTransactions transactions;

        (* Obtener la transacción con el monto más grande *)
        val greaterAmountAccount = greaterAmountTransaction transactions;

        val lesserAmountAccount = lesserAmountTransaction transactions;

        (* Imprimir los resultados *)
        val _ = (
            printLine "\nCantidad de transacciones por tipo";
            printLine "-------------------------------------";
            printLine ("Depositos = " ^ Int.toString numDepositos);
            printLine ("Retiros = " ^ Int.toString numRetiros);
            printLine ("Transferencias = " ^ Int.toString numTransferencias);
            printLine "--------------------------------------\n";

            (* Imprimir la transacción con el monto más grande *)
            printLine "Transacción con el monto más grande:";
            printLine "-------------------------------------";
            printLine ("Cuenta = " ^ List.nth(greaterAmountAccount, 0));
            printLine ("Fecha = " ^ List.nth(greaterAmountAccount, 1));
            printLine ("Tipo = " ^ List.nth(greaterAmountAccount, 2));
            printLine ("Monto = " ^ List.nth(greaterAmountAccount, 3));

            (*Verifica si existe una cuenta destino*)
            if length(greaterAmountAccount) > 4 then
                printLine ("Cuenta destino = " ^ List.nth(greaterAmountAccount, 4))
              else
                printLine "Cuenta disponible = Ninguna";
            
            printLine "--------------------------------------\n";

            (* Imprimir la transacción con el monto más pequeño *)
            printLine "Transacción con el monto más pequeño:";
            printLine "-------------------------------------";
            printLine ("Cuenta = " ^ List.nth(lesserAmountAccount, 0));
            printLine ("Fecha = " ^ List.nth(lesserAmountAccount, 1));
            printLine ("Tipo = " ^ List.nth(lesserAmountAccount, 2));
            printLine ("Monto = " ^ List.nth(lesserAmountAccount, 3));

            (*Verifica si existe una cuenta destino*)
            if length(lesserAmountAccount) > 4 then
                printLine ("Cuenta destino = " ^ List.nth(lesserAmountAccount, 4))
              else
                printLine "Cuenta disponible = Ninguna";
            
            printLine "--------------------------------------\n"
        )
    in
        ()
    end;

(* Función para salir del programa *)
fun exit () =
    OS.Process.exit (Word32.fromInt 0);

(* Función para el menú de creador *)
fun creatorMenu () = 
    let
        val _ = printLine "\nMenú Creador:";
        val _ = printLine "1. Añadir nuevo registro";
        val _ = printLine "2. Limpiar indice";
        val _ = printLine "3. Volver";
        val _ = print "Elija una opción: ";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val opcion = limpiar_cambios_linea temp;
    in
        case opcion of
             "1" => (addRegister (); creatorMenu ())  
           | "2" => (cleanIndex(); creatorMenu ())  
           | "3" => printLine "Volviendo al menú principal..."
           | _   => (printLine "Opción inválida, intente de nuevo"; creatorMenu ())
    end;

(* Función para el menú de analizador *)
fun analyzerMenu () =
    let
        val _ = printLine "\nMenú Analizador:";
        val _ = printLine "1. Mostrar top";
        val _ = printLine "2. Informe de actividades sospechosas";
        val _ = printLine "3. Transacciones por cuenta";
        val _ = printLine "4. Cantidad de transacciones por tipo";
        val _ = printLine "5. Resumen";
        val _ = printLine "6. Volver";
        val _ = print "Elija una opción: ";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val opcion = limpiar_cambios_linea temp;
    in
        case opcion of
             "1" => (showTop (); analyzerMenu ())  
           | "2" => (suspiciousReport(); analyzerMenu ())  
           | "3" => (transactionsPerAccount(); analyzerMenu ())  
           | "4" => (transactionsQuantity(); analyzerMenu ())  
           | "5" => (summary(); analyzerMenu ())  
           | "6" => printLine "Volviendo al menú principal..."
           | _   => (printLine "Opción inválida, intente de nuevo"; analyzerMenu ())
    end;

(* Función para el menú principal *)
fun mainMenu () =
    let
        val _ = printLine "\nMenú Principal:";
        val _ = printLine "1. Creador";
        val _ = printLine "2. Analizador";
        val _ = printLine "3. Salir";
        val _ = print "Elija una opción: ";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val opcion = limpiar_cambios_linea temp;
    in
        case opcion of
             "1" => (creatorMenu (); mainMenu ())  
           | "2" => (analyzerMenu(); mainMenu ())  
           | "3" => printLine "Saliendo del programa..."
           | _   => (printLine "Opción inválida, intente de nuevo"; mainMenu ())
    end;

(* Función principal *)
fun main () = (mainMenu (); exit ());
