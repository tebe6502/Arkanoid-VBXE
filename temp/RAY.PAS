unit ray;
interface
uses fix,_graph,maping;

var x_pos,y_pos:TFix;
    pos_angle:integer;

      procedure CutRays;

implementation
const c1=40*4;
      c2=360*4;

var dang:byte;
    line,angle:integer;
    x_sel,y_sel:byte;

      procedure X_cut(var xi,yi:TFix);
      var y_step,x_step:TFix;
                begin
                Y_Step:=Fix_Tg(angle);x_step:=$100;
                xi:=fix_round(X_pos);yi:=y_pos-(fix_mul(x_sel,y_step));
                case  dang      of
                                0,3:
                                  repeat
                                  xi:=xi+x_step;yi:=yi+y_step;
                                  until (map(xi,yi)>0);
                                1,2:
                                  repeat
                                  xi:=xi-x_step;yi:=yi-y_step;
                                  until (map(xi-1,yi)>0)or(yi>$1000);
                      end;
                end;

      procedure Y_cut(var xi,yi:TFix);
      var y_step,x_step:TFix;
                begin
                X_Step:=Fix_Div(Fix_Cos(angle),Fix_Sin(angle));Y_step:=$100;
                yi:=Fix_Round(y_pos);xi:=X_pos-abs(fix_mul(y_sel,x_step));
                case  dang       of
                                0:begin
                                  repeat
                                  xi:=xi+x_step;yi:=yi+y_step;
                                  until (map(xi,yi)>0){or(xi<0)};end;
                                1:begin
                                  xi:=X_pos+(fix_mul(y_sel,abs(x_step)));
                                  repeat
                                  xi:=xi+x_step;yi:=yi+y_step;
                                  until (map(xi,yi)>0)or(xi<0);end;
                                2,3:begin
                                  repeat
                                  xi:=xi-x_step;yi:=yi-y_step;
                                  until (map(xi,yi-1)>0)or(xi>$1000);end;
                      end;
                end;
      procedure CutRay;
               var xx,yx,xy,yy,x_lon,y_lon,lon:TFix;
                   y:integer;
               begin
               x_sel:=fix_drob(x_pos);
               y_sel:=fix_drob(y_pos);

               dang:=angle div 360;
               X_cut(xx,yx);Y_Cut(xy,yy);
               xx:=abs(xx-x_pos);yy:=abs(yy-y_pos);
{               xx:=xx-x_sel;{yy:=yy-y_sel;}

               X_lon:=abs(fix_div(xx,fix_cos(angle)));
               y_lon:=abs(fix_div(yy,fix_sin(angle)));
               if x_lon<y_lon then lon:=X_lon else lon:=y_lon;
               y:= 150*256 div (fix_mul(lon,abs(fix_cos(abs(pos_angle-angle)))));

               if x_lon<y_lon then linetex(line,100-y,100+y,fix_drob(yx)shr 2,sprite[1]) else
                                   linetex(line,100-y,100+y,fix_drob(xy)shr 2,sprite[1]);
               end;
      procedure CutRays;
      begin
      if pos_Angle<0 then pos_angle:=c2+pos_angle;
      if pos_angle>=c2 then pos_angle:=pos_angle-c2;

      angle:=pos_angle-c1;
      if Angle<0 then angle:=c2+angle;
      for line:=0 to 319 do
                         begin
                         cutray;
                         inc(angle);
                         if angle>=c2 then angle:=angle-c2;
                         end;
      end;

end.