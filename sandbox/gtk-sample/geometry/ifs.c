#include <gtk/gtk.h>
#include <time.h>
#include "matrix.c"

#define PICT_WIDTH   300
#define PICT_HEIGHT  510

int MAX=30000;
GdkGC *g_gc = NULL;
GtkWidget *g_window;

enum { false, true };

gint destroyapp (GtkWidget *, gpointer);
void SetColor(gushort, gushort, gushort);
void draw_picture(GdkDrawable *);
gint expose_event(GtkWidget *, GdkEventExpose *);

int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *drawing_area;

  if(argc == 2)
    MAX=atoi(argv[1]);

  /* --- GTK initialization --- */
  gtk_init( &argc, &argv );

  /* --- Create the top level window --- */
  g_window = window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  /* --- Show the label  --- */
  gtk_widget_show(window);
  gtk_signal_connect(GTK_OBJECT(window), "destroy",
         GTK_SIGNAL_FUNC(destroyapp), NULL);

  drawing_area = gtk_drawing_area_new();
  gtk_drawing_area_size( GTK_DRAWING_AREA(drawing_area),
       PICT_WIDTH, PICT_HEIGHT);
  gtk_signal_connect(GTK_OBJECT(drawing_area), "expose_event",
         (GtkSignalFunc) expose_event, NULL);
  gtk_widget_set_events(drawing_area, GDK_EXPOSURE_MASK);
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
  /* --- Shut down gtk event loop processing --- */
  gtk_main_quit ();
}

void SetColor(gushort r, gushort g, gushort b)
{
  GdkColor  color;

  color.red = r, color.green = g, color.blue = b;
  gdk_color_alloc(gdk_colormap_get_system(),&color);
  gdk_gc_set_foreground(g_gc, &color);
}

void draw_picture(GdkDrawable *gd)
{
  int i;
  double WIDTH,HEIGHT,OriginX,OriginY,ScaleX,ScaleY;

  double Max_X,Min_X,Max_Y,Min_Y;
  double r;
  gT_M31 p;
  gT_M33 m1,m2,m3,m4;
  
  Max_X=50;
  Min_X=-50;
  Max_Y=160;
  Min_Y=-10;
  
  // 論理座標系の描画領域
  WIDTH=Max_X-Min_X;
  HEIGHT=Max_Y-Min_Y;
  
  // 論理座標系の中心座標
  OriginX=WIDTH/2.0-(Max_X+Min_X)/2;
  OriginY=HEIGHT/2.0+(Max_Y+Min_Y)/2;
  
  // スケールファクタの計算
  ScaleX=(double)PICT_WIDTH/WIDTH,
  ScaleY=(double)PICT_HEIGHT/HEIGHT;

  // 変換行列の設定
  m1=gM33Set(0.2,-0.26,0,0.23,0.22,24,0,0,1);
  m2=gM33Set(-0.15,0.28,0,0.26,0.24,6.6,0,0,1);
  m3=gM33Set(0,0,0,0,0.16,0,0,0,1);
  m4=gM33Set(0.85,0.04,0,-0.04,0.85,24,0,0,1);
  
  // 初期値の設定
  p=gM31Set(0,0,1);
  
  /* 背景を白に設定 */
  SetColor( 0xffff, 0xffff, 0xffff);
  gdk_gc_set_line_attributes( g_gc, 1, GDK_LINE_SOLID,
           GDK_CAP_BUTT, GDK_JOIN_ROUND );
  gdk_draw_rectangle(gd,g_gc,TRUE,0,0,300,510);

  // 点の色を青に設定
  SetColor( 0x0000, 0x0000, 0xffff);
  gdk_gc_set_line_attributes( g_gc, 1, GDK_LINE_SOLID,
           GDK_CAP_BUTT, GDK_JOIN_ROUND );

  // 乱数の初期化
  srand(time(NULL));

  // 乱数の値によって変換行列を選択
  for(i=0;i<MAX;i++){

    r=rand()/(double)RAND_MAX;

    if(0<=r&&r<0.065)
      p=gM33xM31(m1,p);
    if(0.065<=r&&r<0.13)
      p=gM33xM31(m2,p);
    if(0.13<=r&&r<0.150)
      p=gM33xM31(m3,p);
    if(0.150<r&&r<=1)
      p=gM33xM31(m4,p);

    // 点の描画
    gdk_draw_point(gd,g_gc,(OriginX+p.v[0])*ScaleX,(OriginY-p.v[1])*ScaleY);
  }
}
gint expose_event(GtkWidget *widget, GdkEventExpose *event)
{
  draw_picture(widget->window);
  return FALSE;
}
