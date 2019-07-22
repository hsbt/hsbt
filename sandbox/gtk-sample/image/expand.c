#include <gtk/gtk.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../bmptype.h"

GdkGC *g_gc = NULL;
GtkWidget *g_window;
double scale_x=1.0, scale_y=1.0;
char fname1[128], fname2[128];

int hist_min,hist_max;
double scale;

FILE *fp1, *fp2;
BMPFILE_HEADER bmpf_h;
BMPINFO_HEADER bmpi_h;
int pict_width, pict_height;
unsigned char bmp_ctable[256][3];
guchar *rgbbuf,*rgbbuf2;

int hist_func(int);
int hist_ctable[256][3];

gint destroyapp (GtkWidget *, gpointer);
void draw_picture(GdkDrawable *);
gint expose_event(GtkWidget *, GdkEventExpose *);
void read_bmpfile(FILE *), write_bmpfile(FILE *);
char *chk_extention(char *, char *);
FILE *open_bmpfile(char *, BMPFILE_HEADER *, BMPINFO_HEADER *);
FILE *open_bmpfile_w(char *, int, int, int, BMPFILE_HEADER *, BMPINFO_HEADER *);
long int get_data(FILE *, Endian, int);
void put_data(FILE *, Endian, int, long int);

int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *drawing_area;
  int mode;

  /* --- GTK initialization --- */
  gtk_init( &argc, &argv );
  gdk_rgb_init();

  /* --- Open BMP file and Read headers --- */
  if (argc>1) strcpy(fname1, argv[1]);
  else { printf("入力画像ファイル名： "); gets(fname1); }

  if (argc>2) strcpy(fname2, argv[2]);
  else { printf("出力画像ファイル名： "); gets(fname2); }

  if (argc>3) mode = atoi(argv[3]);
  else {
    printf("何バイト／ピクセルで出力しますか？ [8 or 24] ");
    scanf("%d", &mode);
  }

  fp1=open_bmpfile(fname1, &bmpf_h, &bmpi_h);
  pict_width=bmpi_h.biWidth; pict_height=bmpi_h.biHeight;

  rgbbuf=(guchar *) malloc(pict_width * pict_height * 3);
  if (rgbbuf==NULL) {
    printf("Can't allocate rgbbuf\n"); exit(1);
  }
  rgbbuf2=(guchar *) malloc(pict_width * pict_height * 3);
  if (rgbbuf==NULL) {
    printf("Can't allocate rgbbuf\n"); exit(1);
  }
  read_bmpfile(fp1);

  fp2=open_bmpfile_w(fname2, pict_width, pict_height, mode, &bmpf_h, &bmpi_h);
  write_bmpfile(fp2);

  /* --- Create the top level window --- */
  g_window = window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  /* --- Show the label  --- */
  gtk_widget_show(window);
  gtk_signal_connect(GTK_OBJECT(window), "destroy",
		     GTK_SIGNAL_FUNC(destroyapp), NULL);

  drawing_area = gtk_drawing_area_new();
  gtk_drawing_area_size( GTK_DRAWING_AREA(drawing_area),
			 pict_width, pict_height);
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
  int x, y, ct, i, r, g, b, x2, y2;

  printf("scale: (%f, %f)\n", scale_x, scale_y);
  if (scale_x<scale_y) scale_y=scale_x;
  else scale_x=scale_y;

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
	  // この部分を埋めよ。
    		b = fgetc(fp); g = fgetc(fp); r = fgetc(fp);
    		break;
      }

      x2=x*scale_x; y2=(-y+pict_height-1)*scale_y;
      rgbbuf[(x2+y2*pict_width)*3+0] = r;
      rgbbuf[(x2+y2*pict_width)*3+1] = g;
      rgbbuf[(x2+y2*pict_width)*3+2] = b;

      ct += (bmpi_h.biBitCount/8);                   /* バイト数のカウント */
    }
    ct %= 4;     /* 画面水平１ラインは、必ず４の倍数バイトのデータ */
    /* パディングデータの読みとばし */
    if (ct) for (i = ct; i < 4; i ++) fgetc(fp);
  }

  	for(i=0;i<256;i++)
	if(hist_ctable[i][0] > 0||hist_ctable[i][1] > 0||hist_ctable[i][2] > 0){
	  hist_min=i;
	  break;
	}
  	for(i=255;i>=0;i--)
	if(hist_ctable[i][0] > 0||hist_ctable[i][1] > 0||hist_ctable[i][2] > 0){
	  hist_max=i;
	  break;
	}

	for(i=0;i<3;i++)
	scale=(double)(hist_max-hist_min);

  for (y = 0; y < pict_height; y ++) {
    for (x = 0; x < pict_width; x ++) {
      r=rgbbuf[(x2+y2*pict_width)*3+0];
      g=rgbbuf[(x2+y2*pict_width)*3+1];
      b=rgbbuf[(x2+y2*pict_width)*3+2];
      x2=x*scale_x; y2=(-y+pict_height-1)*scale_y;
      rgbbuf2[(x2+y2*pict_width)*3+0] = hist_func(r);
      rgbbuf2[(x2+y2*pict_width)*3+1] = hist_func(g);
      rgbbuf2[(x2+y2*pict_width)*3+2] = hist_func(b);

    }
  }

  fclose(fp);
}

void write_bmpfile(FILE *fp)
{
  int x, y, ct, i, r, g, b, x2, y2;

  for (y = 0; y < pict_height; y ++) {
    ct = 0;				/* バイト数カウンタのクリア */
    for (x = 0; x < pict_width; x ++) {
      x2 = x*scale_x; y2 = (-y+pict_height-1)*scale_y;
      r = rgbbuf2[(x2+y2*pict_width)*3+0];
      g = rgbbuf2[(x2+y2*pict_width)*3+1];
      b = rgbbuf2[(x2+y2*pict_width)*3+2];

      switch (bmpi_h.biBitCount) {
      case 24:
        fputc(b, fp); fputc(g, fp); fputc(r, fp);
        break;
      case 8:
        fputc(b, fp); fputc(g, fp); fputc(r, fp);
        break;
	  // この部分を埋めよ。
      }
      ct += (bmpi_h.biBitCount/8);	/* バイト数のカウント */
    }
    ct %= 4;				/* 画面水平１ラインは、必ず４の倍数バイトのデータ */
    /* パディングデータの書き込み */
    if (ct) for (i = ct; i < 4; i ++) fputc(0, fp);
  }

  fclose(fp);
}

void draw_picture(GdkDrawable *gd)
{
  gdk_draw_rgb_image (gd, g_gc,
		      0, 0, pict_width, pict_height,
		      GDK_RGB_DITHER_MAX, rgbbuf2, pict_width * 3);
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

// Windowsビットマップファイルのオープン（書き込み）
FILE *open_bmpfile_w(char *file, int xsize, int ysize, int mode, BMPFILE_HEADER *bmph, BMPINFO_HEADER *bmpi)
{
  FILE *fp;
  int i;
  long int lsize;
  unsigned char r, g, b;
  char *fname;

  fname=chk_extention(file, "bmp");
  if ((fp=fopen(fname, "wb"))==NULL) {
    fprintf(stderr, "\"%s\" をオープンできません。\n", fname); exit(1);
  }

  /* 画面水平１ライン当たりのビットマップデータのバイト数 */
  lsize = xsize * mode/8 + ((4-((xsize * mode/8) % 4)) % 4);

  /* ビットマップファイルヘッダーの設定 */
  bmph->type = 0x4D42;                         /* "BM" */
  bmph->ofsbits = 54L +(mode==24 ? 0L : 1024L);
  bmph->size = bmph->ofsbits + ysize * lsize;
  bmph->rsv1 = bmph->rsv2 = 0;

  /* ビットマップインフォヘッダーの設定 */
  bmpi->biSize = 40;
  bmpi->biWidth = xsize;
  bmpi->biHeight = ysize;
  bmpi->biPlanes = 1;
  bmpi->biBitCount = mode;
  bmpi->biCompression = 0L;
  bmpi->biSizeImage = 0L;
  bmpi->biXPelsPerMeter = 0L;
  bmpi->biYPelsPerMeter = 0L;
  bmpi->biClrUsed = 0L;
  bmpi->biClrImportant = 0L;

  /* ビットマップファイルヘッダーの書き込み */
  put_data(fp, Little, 2, bmph->type);    /* 2 */
  put_data(fp, Little, 4, bmph->size);    /* 4 */
  put_data(fp, Little, 2, bmph->rsv1);    /* 2 */
  put_data(fp, Little, 2, bmph->rsv2);    /* 2 */
  put_data(fp, Little, 4, bmph->ofsbits); /* 4 ( sum : 14 ) */

  /* ビットマップインフォヘッダーの書き込み */
  put_data(fp, Little, 4, bmpi->biSize);          /* 4 */
  put_data(fp, Little, 4, bmpi->biWidth);         /* 4 */
  put_data(fp, Little, 4, bmpi->biHeight);        /* 4 */
  put_data(fp, Little, 2, bmpi->biPlanes);        /* 2 */
  put_data(fp, Little, 2, bmpi->biBitCount);      /* 2 */
  put_data(fp, Little, 4, bmpi->biCompression);   /* 4 */
  put_data(fp, Little, 4, bmpi->biSizeImage);     /* 4 */
  put_data(fp, Little, 4, bmpi->biXPelsPerMeter); /* 4 */
  put_data(fp, Little, 4, bmpi->biYPelsPerMeter); /* 4 */
  put_data(fp, Little, 4, bmpi->biClrUsed);       /* 4 */
  put_data(fp, Little, 4, bmpi->biClrImportant);  /* 4  ( sum : 40 ) */
  /************* Total 54 bytes *****************/

  if (mode == 8) {
    /* カラーテーブルの変更・書き込み */
    for (i = 0; i < 256; i ++) {
      // この部分を埋めよ。
      // r =
      // g =
      // b =
      fputc(b, fp); fputc(g, fp); fputc(r, fp); fputc(0, fp);
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

void put_data(FILE *fp, Endian ed, int n, long int ldata)
{
  unsigned char c[4];
  int i, p;

  if (ed==Little) p=0;
  else p=n-1;

  for (i=0; i<n; i++) {
    c[p] = (ldata>>8*i) & 0xff;
    p+=ed;
  }

  for (i=0; i<n; i++) fputc(c[i], fp);
}
int hist_func(int cl)
{
return (cl-hist_min)*(256.0/scale);
}
