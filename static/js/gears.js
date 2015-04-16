


function ToothPath(size){
    size = size * 5;
    var height = size;
    var width = size * 0.8;
    var bump_width = size * 0.05;
    var bump_height = size * 0.60;
    var crown_width = size * 0.23;

    var path = testLine([
        [0,0],
        [0, -bump_height/2,                           bump_width, -bump_height],
        [crown_width/2, ((bump_height-height)/2)-bump_height,      crown_width, -height],
        [width/2, -height,                                       width-crown_width, -height],
        [width-(crown_width/2), ((bump_height-height)/2)-bump_height,  width-bump_width, -bump_height],
        [width, -bump_height/2,                                   width, 0]
    ]) 
    return path
}


function Tooth(paper, size, color) {
  
    size = size * 5;
    var height = size;
    var width = size * 0.8;
    var bump_width = size * 0.05;
    var bump_height = size * 0.60;
    var crown_width = size * 0.23;

    var path = testLine([
        [0,0],
        [0, -bump_height/2,                           bump_width, -bump_height],
        [crown_width/2, ((bump_height-height)/2)-bump_height,      crown_width, -height],
        [width/2, -height,                                       width-crown_width, -height],
        [width-(crown_width/2), ((bump_height-height)/2)-bump_height,  width-bump_width, -bump_height],
        [width, -bump_height/2,                                   width, 0]

        ]) 

    tooth = paper.path(path).attr({
        fill: color, 
        stroke: "black"
    });
    return tooth;
}



function Gears(ID, width, height) {
    var my = this
    if (width == null) {width = 300}
    if (height == null) {height = 300}
    my.paper = new Raphael(document.getElementById(ID), width, height); 
    /*fog = my.paper.rect(0, 0, width, height)
                  .attr({"fill": "rgb(255,255,255)",
                         "opacity":0.9,
                         "stroke":"rgba(255,255,255)",
                         })*/

    //fog.toFront();

    var invertedgear = new Gear(my.paper, 134.5, 161, 37, 1.5, 0, 1, true);
    var gear = new Gear(my.paper, 150.5, 150.5, 11, 1.5);

    gear.style(gearStyle);

    gear.add(19, 143, 8);
    gear.add(11, 242, 10);
    gear.add(7, 0, -27);


    gear.children[0].style(gearStyle);
    gear.children[1].style(gearStyle);
    gear.children[2].style(gearStyle);

    var rotate = function(){
        paths = gear.rotatePath(invertedgear.syncRotate(gear, 360)*-1, null, null, "R360,134.5,161 ")
        time = 40000;
        gear.set.animate(paths["parent"], time);
        gear.children[0].set.animate(paths[0], time);
        gear.children[1].set.animate(paths[1], time);
        gear.children[2].set.animate(paths[2], time);
       
    }
    setTimeout(rotate, 2000);



}



function Gear(paper, x, y, size, toothSize, offset, direction, inverted){
    var gear = this;

    gear.x = x;
    gear.y = y;
    gear.size = size;
    gear.set = paper.set();

    if (offset == null){offset = 0}
    gear.offset = offset;

    if (toothSize == null){toothSize = 1}
    gear.toothSize = toothSize;

    if (direction == null){direction = 1}
    gear.direction = direction;

    if (inverted == null){inverted = false}
    gear.inverted = inverted;

    
    
    gear.diameter = size * toothSize;
    gear.circum = gear.diameter*Math.PI;
    gear.effectiveDiameter = ((toothSize*5)/2)+gear.diameter;
    gear.ecircumference = gear.effectiveDiameter*Math.PI;
    

    var strokeColor = "#CCCCCC";
    //var fillColor = "#FAFAFA";
    var fillColor = "#FFFFFF";

    if (gear.inverted){
        gear.style = {
            "fill":fillColor,
            "stroke":"#333333",
            //"stroke-dasharray":".",
            "stroke-width":0.5,

        }
        paper.circle(gear.x, gear.y, gear.diameter+(toothSize*15))
             .attr({"stroke": strokeColor,
                "fill": fillColor,

              })

        gear.wheel = paper.circle(gear.x, gear.y, gear.diameter+(toothSize*8.5))
                      .attr(gear.style)
                      .attr("fill", "#EEEEEE")
        gear.set.push(gear.wheel)
        gear.path = function() {
            var increase = (360 / size);
            var angle = 0;
            var path = '';
            for (var i = 0; i < size; i++) {
                path = path + " " + paper.raphael.transformPath(ToothPath(gear.toothSize), 
                                    "r" + [175,0,0] +
                                    " T" + [gear.x-((toothSize*5*0.8)/2), gear.y-gear.diameter-(toothSize*10.0)+0.9] + 
                                    " R" + [angle+offset, gear.x, gear.y]                                     
                                    );
                angle += increase;
            }
            return path;
        }()
        gear.set.push(paper.path(gear.path)
                           .attr(gear.style));


    } else {
        gear.style = {
            //"fill":"#CCCCCC",
            "fill":fillColor,
            "stroke":"#333333",
            "stroke-width":0.5,
        }
        gear.wheel = paper.circle(gear.x, gear.y, gear.diameter)
                      .attr(gear.style);
        gear.set.push(gear.wheel)
        gear.path = function() {
            var increase = (360 / size);
            var angle = 0;
            var path = '';
            for (var i = 0; i < size; i++) {
                path = path + " " + paper.raphael.transformPath(ToothPath(gear.toothSize), 
                                    "T" + [gear.x-((toothSize*5*0.8)/2), gear.y-gear.diameter+0.9] + 
                                    "R" + [angle+offset, gear.x, gear.y]);
                angle += increase;
            }
            return path;
        }()
        gear.set.push(paper.path(gear.path)
                           .attr(gear.style));
    }

    


    gear.style = function(style){
        gear.set.push(style(paper, gear));
    }
    

    gear.children = []
    gear.sync = function(child){
        gear.children.push(child)
    }

    gear.add = function(size, angle, innerangle){
        if (angle == null){angle = 0}
        coords = Rotate(gear.x+gear.diameter+(toothSize*5)+(size*gear.toothSize), gear.y, gear.x, gear.y, angle);
        if (gear.direction == 1){direction = -1} else {direction = 1}

        gear.sync(new Gear(paper, coords["x"],coords["y"], size, gear.toothSize, innerangle, direction))
    }

    gear.rotate = function(angle, duration, x, y){
        var eX = x;
        var eY = y;
        if (x == null){
            eX = gear.x;
            eY = gear.y;
        } 
        gear.set.animate({"transform":"R" + [angle, eX, eY]}, duration);

        console.log(gear.size)
        console.log("R" + [angle, eX, eY])
        for (var i = 0; i < gear.children.length; i++) {
            gear.children[i].rotate(-(gear.diameter/gear.children[i].diameter)*angle, duration, x, y);
        }
    }

    gear.rotatePath = function(angle, x, y, prepend, name){

        var eX = x;
        var eY = y;
        if (x == null){
            eX = gear.x;
            eY = gear.y;
        } 
        if (name == null){
            name = "parent";
        }
        //gear.set.animate({"transform":"R" + [angle, eX, eY]}, duration);
        var transforms = {}
        transforms[name] = {"transform": prepend +"r" + [angle, eX, eY]};
        for (var i = 0; i < gear.children.length; i++) {
            //gear.children[i].rotatePath(-(gear.diameter/gear.children[i].diameter)*angle, x, y, append, i);
            //var path = gear.children[i].rotatePath(-(gear.diameter/gear.children[i].diameter)*angle, x, y, prepend, i);
            var path = gear.children[i].rotatePath(gear.syncRotate(gear.children[i], angle), x, y, prepend, i);
            transforms[i] = path[i]; 

        }

        return transforms
    }
    gear.syncRotate = function(child, angle){
        return -(gear.diameter/child.diameter)*angle
    }


    gear.transform = function(transform, duration){
        gear.set.animate({"transform":Raphael.fullfill(transform, gear)}, duration);
        for (var i = 0; i < gear.children.length; i++) {
            gear.children[i].transform(transform);
        }
    }
    return gear;
}






var gearStyle = function(paper, gear){
    var st = paper.set();
    st.push(paper.circle(gear.x, gear.y, gear.diameter-4)
                  .attr({
                    "fill":"#FAFAFA",
                    "stroke":"#555555",
                    "stroke-width":0.5,
                  }));
    var increase = (360 / 3);
    var angle = 0;
    
    for (var i = 0; i < 3; i++) {
        continue;
        coords = Rotate(gear.x, gear.y+(gear.diameter/2), gear.x, gear.y, angle+gear.offset);
        console.log(coords);
        var circle = paper.circle(coords["x"], coords["y"] , gear.diameter/5);
        //var fill = paper.circle(coords["x"], coords["y"] , (gear.diameter/5)-3);
        circle.attr({
            "fill":"#FFFFFF",
            "stroke":"#555555"
        })
        //fill.attr({
        //    "fill":"white",
        //    "stroke":"#555555"
        //})
        st.push(circle);
        //st.push(fill);
        angle += increase;
    }
    return st;
}

var gearStyle2 = function(paper, gear){
    var st = paper.set();
    st.push(paper.circle(gear.x, gear.y, gear.diameter-8)
                  .attr({
                    "fill":"#FFFFFF",
                    "stroke":"#333333"
                  }));


    var increase = (360 / 5);
    var angle = 0;
    
    for (var i = 0; i < 5; i++) {
        //coords = Rotate(gear.x, gear.y+(gear.diameter/2), gear.x, gear.y, angle+offset)

        var top = gear.diameter/3;
        var bottom = gear.diameter/8-4;

        var waist = gear.diameter-13;


        var line = paper.path(testLine([[gear.x, gear.y+5], 
                                     [gear.x+bottom, gear.y+5, gear.x+bottom, gear.y+25], 
                                     [gear.x+top, gear.y+waist, gear.x, gear.y+gear.diameter-4],
                                     [gear.x-top, gear.y+waist, gear.x-bottom, gear.y+25],
                                     [gear.x-bottom, gear.y+5, gear.x, gear.y+5],

            ]))

        line.attr("path", paper.raphael.transformPath(line.attr("path"), "R" + [angle+gear.offset, gear.x, gear.y]))
        
        line.attr({
            "fill":"#AAAAAA",
            "stroke":"#555555"
        })
        st.push(line);
        angle += increase;
    }

    st.push(paper.circle(gear.x, gear.y, gear.diameter/6)
                  .attr({
                    "fill":"#CCCCCC",
                    "stroke":"#333333"
                  }));
    st.push(paper.circle(gear.x, gear.y, (gear.diameter/8))
                  .attr({
                    "fill":"#FFFFFF",
                    "stroke":"#333333"
                  }));

    return st;
}





function line(coords) {
    var command = "M"
    var note = ""
    for (var i = 0; i < coords.length; i++) {
        note = note + command + coords[i][0].toString() + " " + coords[i][1].toString()
        command = "L"
    }
    return note
}

function curve(coords) {
    var command = "M"
    var note = ""
    for (var i = 0; i < coords.length; i++) {
        if (i == 0){
            note = note + command + coords[i][0].toString() + " " + coords[i][1].toString();
            command = "S";
            continue;
        }

        note = note + command + coords[i][0].toString() + " " + coords[i][1].toString() + " "+ coords[i][2].toString() + " " + coords[i][3].toString()
        
    }
    return note
}


function testLine(coords){

    var command = "M"
    var note = ""
    for (var i = 0; i < coords.length; i++) {
        if (i == 0){
            note = note + command + coords[i][0].toString() + " " + coords[i][1].toString();
            command = "L";
            continue;
        }

        note = note + command + coords[i][0].toString() + " " + coords[i][1].toString() + command + coords[i][2].toString() + " " + coords[i][3].toString()
        
    }
    return note

}

function addLine(path, path2){


}



function fline(coords) {
    var note = []
    for (var i = 0; i < coords.length; i++) {
        note = note.concat(coords[i]);
    }
    return note
}


function extend(destination, source) {
    for (var property in source) {
        if (source.hasOwnProperty(property)) {
            destination[property] = source[property];
        }
    }
    return destination;
};



function Rotate(pointX, pointY, centerX, centerY, angle) {
  // convert angle to radians
  angle = angle * Math.PI / 180.0;
  // get coordinates relative to center
  var dx = pointX - centerX;
  var dy = pointY - centerY;
  // calculate angle and distance
  var a = Math.atan2(dy, dx);
  var dist = Math.sqrt(dx * dx + dy * dy);
  // calculate new angle
  var a2 = a + angle;
  // calculate new coordinates
  var dx2 = Math.cos(a2) * dist;
  var dy2 = Math.sin(a2) * dist;
  // return coordinates relative to top left corner
  return { "x": dx2 + centerX, "y": dy2 + centerY };
}