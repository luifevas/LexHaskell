
/*hola mundo*/

//esto es un comentario

int main(void)
{
int p = 2000 ; 
short int q = -120 ; 
unsigned short int r = 121 ; 
float s = 21.566578 ; 
char t = 'r'; 
long u = 5678; 
unsigned long v = 5678; 
long w = -5678; 
int x = -171; 
short y = -71; 
unsigned short z = 99; 
double a = 88.12345; 
float b = -3.245823; 
char Title[50] = "The Making of the Casino Royale";
 
printf("\t--Data type again--\n");
printf("\t-------------------\n");
 
printf("\n1. \"int\" sample: \t\t %d, the data size: %d bytes", p, sizeof(p));
printf("\n2. \"short\" int sample: \t %d, the data size: %d bytes", q, sizeof(q));
printf("\n3. \"unsigned short int\" sample: %d, the data size: %d bytes", r, sizeof(r));
printf("\n4. \"float\" sample: \t\t %.7f, the data size: %d bytes", s, sizeof(s));
printf("\n5. \"char\" sample: \t\t %c, the data size: %d byte", t, sizeof(t));
printf("\n6. \"long\" sample: \t\t %d, the data size: %d bytes", u, sizeof(u));
printf("\n7. \"unsigned long\" sample: \t %d, the data size: %d bytes", v, sizeof(v));
printf("\n8. negative \"long\" sample: \t %d, the data size: %d bytes", w, sizeof(w));
printf("\n9. negative \"int\" sample: \t %d, the data size: %d bytes", x, sizeof(x));
printf("\n10. negative \"short\" sample: \t %d, the data size: %d bytes", y, sizeof(y));
printf("\n11. unsigned \"short\" sample: \t %d, the data size: %d bytes", z, sizeof(z));
printf("\n12. \"double\" sample: \t\t %.4f, the data size: %d bytes", a, sizeof(a));
printf("\n13. negative \"float\" sample: \t %.5f, the data size: %d bytes", b, sizeof(b));
printf("\n14. The NULL terminated string data size: %d bytes\n", sizeof(Title));
 
return 0 ;
} 