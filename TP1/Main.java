
import java.io.*;

public class Main {

    public static boolean pair (int a) {
	boolean r;

	if (a == 0)
	    r = true;
	else 
	    r = impair(a - 1);
	return r;
    }
    
    public static boolean impair (int a) {
	boolean r;

	if (a == 0)
	    r = false;
	else 
	    r = pair(a - 1);
	return r;
    }
    
    public static void main (String [] args) throws java.io.IOException {

	BufferedReader br = new BufferedReader(new InputStreamReader (System.in));

	int n;
	System.out.print("Donner nombre > 0: ");
	n = Integer.valueOf(br.readLine()).intValue();
	if (pair(n))
	    System.out.println("true");
	else 
	    System.out.println("false");
    }
    
}

