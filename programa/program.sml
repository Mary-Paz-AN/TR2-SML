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

fun transactionsPerAccount () = (print "Transacciones por cuenta\n");

fun transactionsQuantity () = (print "Cantidad de transacciones\n");

fun summary () = (print "Resumen\n");

(* Función para salir del programa *)
fun exit () =
    OS.Process.exit (Word32.fromInt 0);

(* Función para el menú de creador *)
fun creatorMenu () = 
    let
        val _ = print "\nMenú Creador:\n";
        val _ = print "1. Añadir nuevo registro\n";
        val _ = print "2. Limpiar indice\n";
        val _ = print "3. Volver\n";
        val _ = print "Elija una opción: ";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val opcion = limpiar_cambios_linea temp;
    in
        case opcion of
             "1" => (addRegister (); creatorMenu ())  
           | "2" => (cleanIndex(); creatorMenu ())  
           | "3" => print "Volviendo al menú principal...\n"
           | _   => (print "Opción inválida, intente de nuevo\n"; creatorMenu ())
    end;

(* Función para el menú de analizador *)
fun analyzerMenu () =
    let
        val _ = print "\nMenú Analizador:\n";
        val _ = print "1. Mostrar top\n";
        val _ = print "2. Informe de actividades sospechosas\n";
        val _ = print "3. Transacciones por cuenta\n";
        val _ = print "4. Cantidad de transacciones por tipo\n";
        val _ = print "5. Resumen\n";
        val _ = print "6. Volver\n";
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
           | "6" => print "Volviendo al menú principal...\n"
           | _   => (print "Opción inválida, intente de nuevo\n"; analyzerMenu ())
    end;

(* Función para el menú principal *)
fun mainMenu () =
    let
        val _ = print "\nMenú Principal:\n";
        val _ = print "1. Creador\n";
        val _ = print "2. Analizador\n";
        val _ = print "3. Salir\n";
        val _ = print "Elija una opción: ";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val opcion = limpiar_cambios_linea temp;
    in
        case opcion of
             "1" => (creatorMenu (); mainMenu ())  
           | "2" => (analyzerMenu(); mainMenu ())  
           | "3" => print "Saliendo del programa...\n"
           | _   => (print "Opción inválida, intente de nuevo\n"; mainMenu ())
    end;

(* Función principal *)
fun main () = (mainMenu (); exit ());
