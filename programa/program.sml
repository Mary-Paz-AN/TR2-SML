fun limpiar_cambios_linea str = (if (str = "")
        then ""
    else if (String.substring(str, 0, 1) = "\n") (*String.substring(str,  size(str)-1);*)
        then limpiar_cambios_linea(String.substring(str, 1, (size str) - 1))
    else
        String.substring(str, 0, 1) ^
        limpiar_cambios_linea(String.substring(str, 1, (size str) - 1))
        );

fun mainMenu () =
    let
        (* Función para mostrar el menú y obtener la opción del usuario *)
        val temp = print "\nMenú Principal:\n";
        val temp = print "1. Creador\n";
        val temp = print "2. Analizador\n";
        val temp = print "Elija una opción (1 o 2): ";
        val temp = valOf(TextIO.inputLine TextIO.stdIn);
        val opcion = limpiar_cambios_linea temp;
    in
        opcion
    end;

(*Exits the program*)
fun exit() =
    let
        val _ = OS.Process.exit (Word32.fromInt 0)
    in
        ()
    end;

(*Main function for the program*)
fun main () = 
    let
        val opcionMP = mainMenu ();

        if 
    in
        ()
    end;
