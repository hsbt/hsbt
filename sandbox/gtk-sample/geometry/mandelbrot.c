#include <gtk/gtk.h>
#include "matrix.c"

#define PICT_WIDTH   400
#define PICT_HEIGHT  400

GdkGC *g_gc = NULL;
GtkWidget *g_window;
double WIDTH,HEIGHT;

// 論理座標系の描画領域
double r_Min,r_Max,i_Min,i_Max;

enum { false, true };

gint destroyapp (GtkWidget *, gpointer);
void SetColor(gushort, gushort, gushort);
void draw_picture(GdkDrawable *);
gint expose_event(GtkWidget *, GdkEventExpose *);

int main(int argc, char *argv[])
{
  GtkWidget *window;
  GtkWidget *drawing_area;

  /* --- GTK initialization --- */
  gtk_init( &argc, &argv );

  // 引数を実数に変換
  if(argc == 5){
    r_Min=atof(argv[1]);
    r_Max=atof(argv[2]);
    i_Min=atof(argv[3]);
    i_Max=atof(argv[4]);
  }else{
    r_Min=-2.2;
    r_Max=0.5;
    i_Min=-1.35;
    i_Max=1.35;
  }

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
  g_print("Quitting ...\n");
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
  int k,km=300,i,MAX,r,g,b;
  gT_CX z,c;

  // スケールファクタと中心点
  double OriginX,OriginY;
  double ScaleX,ScaleY;
  double x,y;
  
  // 論理座標系の描画領域
  WIDTH=r_Max-r_Min;
  HEIGHT=i_Max-i_Min;

  // 論理座標系の中心座標
  OriginX=WIDTH/2.0-(r_Max+r_Min)/2;
  OriginY=HEIGHT/2.0+(i_Max+i_Min)/2;

  // スケールファクタの計算
  ScaleX=(double)PICT_WIDTH/WIDTH,
  ScaleY=(double)PICT_HEIGHT/HEIGHT;

  /* 色を黒に設定 */
  SetColor( 0x0000, 0x0000, 0x0000);
  gdk_gc_set_line_attributes( g_gc, 1, GDK_LINE_SOLID,
           GDK_CAP_BUTT, GDK_JOIN_ROUND );
  gdk_draw_rectangle(gd,g_gc,TRUE,0,0,PICT_WIDTH,PICT_HEIGHT);

  SetColor( 0xffff, 0xffff, 0xffff);
  gdk_gc_set_line_attributes( g_gc, 1, GDK_LINE_SOLID,
           GDK_CAP_BUTT, GDK_JOIN_ROUND );

  for(c.re = r_Min ; c.re < r_Max ; c.re+=0.95/ScaleX)
    for(c.im = i_Min ; c.im < i_Max ; c.im+=0.95/ScaleY){
      z.re=0;
      z.im=0;
      for(k=0;k<km;k++){
        x=(z.re*z.re-z.im*z.im)+c.re;
        y=2*z.re*z.im+c.im;
        if((x*x+y*y)>4){
          r=k%5;g=k%3;b=k%2;
          SetColor(0xfff-r<<13,0xfff-g<<14,0xfff-b<<15);
          gdk_draw_point(gd,g_gc, (OriginX+c.re)*ScaleX,(OriginY-c.im)*ScaleY);
          break;
        }else{
          z.re=x;
          z.im=y;
        }
      }
    }
}
gint expose_event(GtkWidget *widget, GdkEventExpose *event)
{
  draw_picture(widget->window);
  return FALSE;
}
