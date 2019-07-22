#include <gtk/gtk.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../bmptype.h"

GdkGC *g_gc = NULL;
GtkWidget *g_window;

double scale_x=1.0, scale_y=1.0;
char fname1[128];
FILE *fp1;

BMPFILE_HEADER bmpf_h;
BMPINFO_HEADER bmpi_h;

int pict_width, pict_height;
unsigned char bmp_ctable[256][3];
int hist_ctable[256][3];
guchar *rgbbuf;

gint destroyapp (GtkWidget *, gpointer);
void draw_picture(GdkDrawable *);
gint expose_event(GtkWidget *, GdkEventExpose *);
void read_bmpfile(FILE *), write_bmpfile(FILE *);

char *chk_extention(char *, char *);
FILE *open_bmpfile(char *, BMPFILE_HEADER *, BMPINFO_HEADER *);
void SetColor(gushort, gushort, gushort);

long int get_data(FILE *, Endian, int);

int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *drawing_area;

  /* --- GTK initialization --- */
  gtk_init( &argc, &argv );
  gdk_rgb_init();

  /* --- Open BMP file and Read headers --- */
  if (argc>1) strcpy(fname1, argv[1]);
  else { printf("入力画像ファイル名： "); gets(fname1); }

  fp1=open_bmpfile(fname1, &bmpf_h, &bmpi_h);

  pict_width=bmpi_h.biWidth; pict_height=bmpi_h.biHeight;

  rgbbuf=(guchar *) malloc(pict_width * pict_height * 3);
  if (rgbbuf==NULL) {
    printf("Can't allocate rgbbuf\n"); exit(1);
  }
  read_bmpfile(fp1);

  pict_width=768;
  pict_height=256;



  /* --- Create the top level window --- */
  g_window = window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  /* --- Show the label  --- */
  gtk_widget_show(window);
  gtk_signal_connect(GTK_OBJECT(window), "destroy",
		     GTK_SIGNAL_FUNC(destroyapp), NULL);

  drawing_area = gtk_drawing_area_new();
  gtk_drawing_area_size( GTK_DRAWING_AREA(drawing_area), pict_width, pict_height);
  gtk_signal_connect(GTK_OBJECT(drawing_area), "expose_event",
		     (GtkSignalFunc) expose_event, NULL);
  gtk_widget_set_events(drawing_area, GDK_EXPOSURE_MASK
	| GDK_BUTTON_PRESS_MASK);
  gtk_container_add(GTK_CONTAINER(window), drawing_area);

  /* --- Show the label  --- */
  gtk_widget_show(drawing_area);

  /* --- Create GC  --- */
  g_gc = gdk_gc_new(window->window);

  gtk_main();
  return 0;
}

gint destroyapp(GtkWidget *widget, gpointer data)
{
  g_print("Quitting ...\n");
  /* --- Shut down gtk event loop processing --- */
  gtk_main_quit ();

  return 0;
}

void read_bmpfile(FILE *fp)
{
  int x, y, ct, i, r, g, b;
  int tp;

  printf("scale: (%f, %f)\n", scale_x, scale_y);
  if (scale_x<scale_y) scale_y=scale_x;
  else scale_x=scale_y;

  for(i=0;i<256;i++){
    hist_ctable[i][0]=0;
    hist_ctable[i][1]=0;
    hist_ctable[i][2]=0;
  }

  for (y = 0; y < pict_height; y ++) {
    ct = 0;                             /* バイト数カウンタのクリア */
    for (x = 0; x < pict_width; x ++) {
      switch (bmpi_h.biBitCount) {
  	case 24:
    		b = fgetc(fp); g = fgetc(fp); r = fgetc(fp);
    		hist_ctable[b][0]++;
    		hist_ctable[g][1]++;
    		hist_ctable[r][2]++;
    		break;
  	case 8:
  		tp=fgetc(fp);
  		b=bmp_ctable[tp][0];
  		g=bmp_ctable[tp][1];
  		r=bmp_ctable[tp][2];
   		break;
      }

      ct += (bmpi_h.biBitCount/8);                   /* バイト数のカウント */
    }
    ct %= 4;     /* 画面水平１ラインは、必ず４の倍数バイトのデータ */
    /* パディングデータの読みとばし */
    if (ct) for (i = ct; i < 4; i ++) fgetc(fp);
  }

  fclose(fp);
}
void draw_picture(GdkDrawable *gd)
{
	int i,j,c_max;

    c_max=hist_ctable[0][0];
    for(i=0;i<256;i++)
  	for(j=0;j<3;j++)
		if(hist_ctable[i][j] > c_max)
			c_max=hist_ctable[i][j];

	scale_y=(double)pict_height/(double)(c_max);
	scale_x=(double)pict_width/256.0;
	for(i=0;i<255;i++){
		SetColor( 0x0000, 0x0000, 0xffff);
		gdk_draw_line(gd,g_gc,
		 (int)(i*scale_x),(int)((c_max-hist_ctable[i][0])*scale_y),
		 (int)((i+1)*scale_x),(int)((c_max-hist_ctable[i+1][0])*scale_y));
		SetColor( 0x0000, 0xffff, 0x0000);
		gdk_draw_line(gd,g_gc,
		 (int)(i*scale_x),(int)((c_max-hist_ctable[i][1])*scale_y),
		 (int)((i+1)*scale_x),(int)((c_max-hist_ctable[i+1][1])*scale_y));
		SetColor( 0xffff, 0x0000, 0x0000);
		gdk_draw_line(gd,g_gc,
		 (int)(i*scale_x),(int)((c_max-hist_ctable[i][2])*scale_y),
		 (int)((i+1)*scale_x),(int)((c_max-hist_ctable[i+1][2])*scale_y));
    }

}
gint expose_event(GtkWidget *widget, GdkEventExpose *event)
{
  draw_picture(widget->window);
  return FALSE;
}

// Windowsビットマップファイルのオープン（読み込み）
FILE *open_bmpfile(char *file, BMPFILE_HEADER *bmph, BMPINFO_HEADER *bmpi)
{
  FILE *fp;
  int i;
  char *fname;

  fname=chk_extention(file, "bmp");
  if ((fp=fopen(fname, "rb")) == NULL) {
    fprintf(stderr, "\"%s\" をオープンできません。", fname);
    exit(1);
  }

//   ビットマップファイルヘッダーの読み込み
  bmph->type = get_data(fp, Little, 2);		// 2
  bmph->size = get_data(fp, Little, 4);		// 4
  bmph->rsv1 = get_data(fp, Little, 2);		// 2
  bmph->rsv2 = get_data(fp, Little, 2);		// 2
  bmph->ofsbits = get_data(fp, Little, 4);	// 4  ( sum : 14 )

// ビットマップインフォヘッダーの読み込み
  bmpi->biSize = get_data(fp, Little, 4);          // 4
  bmpi->biWidth = get_data(fp, Little, 4);         // 4
  bmpi->biHeight = get_data(fp, Little, 4);        // 4
  bmpi->biPlanes = get_data(fp, Little, 2);        // 2
  bmpi->biBitCount = get_data(fp, Little, 2);      // 2
  bmpi->biCompression = get_data(fp, Little, 4);   // 4
  bmpi->biSizeImage = get_data(fp, Little, 4);     // 4
  bmpi->biXPelsPerMeter = get_data(fp, Little, 4); // 4
  bmpi->biYPelsPerMeter = get_data(fp, Little, 4); // 4
  bmpi->biClrUsed = get_data(fp, Little, 4);       // 4
  bmpi->biClrImportant = get_data(fp, Little, 4);  // 4  ( sum : 40 )
  // ************* Total 54 bytes *****************

  printf("画像サイズ：%5ld×%5ld\n", bmpi->biWidth, bmpi->biHeight);
  printf("入力ビットマップデータ：%2d bit/pixel\n", bmpi->biBitCount);

// カラーテーブルの読み込み（8 bit/pixelの場合のみ）
  if (bmpi->biBitCount == 8) {
    for (i = 0; i < 256; i ++) {
      bmp_ctable[i][0] = fgetc(fp);
      bmp_ctable[i][1] = fgetc(fp);
      bmp_ctable[i][2] = fgetc(fp);
      fgetc(fp);
    }
  }

  return fp;
}

// ファイル名の拡張子をチェックする関数
char *chk_extention(char *file, char *ext)
{
  static char fname[80], *p;

  strcpy(fname, file);
  p=fname;
  while (*p!='\0') {
    if (*p++ == '.') return fname;
  }
  strcat(fname, "."); strcat(fname, ext);
  return fname;
}

long int get_data(FILE *fp, Endian ed, int n)
{
  unsigned char c[4];
  long int ldata=0;
  int i, p;

  for (i=0; i<n; i++) c[i]=fgetc(fp);

  if (ed==Little) p=0;
  else p=n-1;

  for (i=0; i<n; i++) {
    ldata |= c[p]<<8*i;
    p+=ed;
  }

  return(ldata);
}
void SetColor(gushort r, gushort g, gushort b)
{
  GdkColor  color;

  color.red = r, color.green = g, color.blue = b;
  gdk_color_alloc(gdk_colormap_get_system(),&color);
  gdk_gc_set_foreground(g_gc, &color);
}
