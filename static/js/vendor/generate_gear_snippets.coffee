
# //GENERATE PLANETARY GEARS
planetary = () ->
    div = $("#gears")
    svg = d3.select(div).append("svg")
        .attr("width", width)
        .attr("height", height)
      .append("g")
        .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")scale(0.18)rotate(40)")
      .append("g")

    frame = svg.append("g")
        .datum({radius: Infinity})

    window.Planetary(frame)
    div
# //END GENERATE PLANETARY GEARS




# //GENERATE GEAR ACCENT
accent = (width, height) ->
    div = $("#gears")
    gearsvg = d3.selectAll(div).append("svg")
        .attr("width", width)
        .attr("height", height)
      .append("g")
        .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")scale(0.25)")
      .append("g");

    gearobj = 
        teeth:12
        offset:[0,0]
        direction:1
    

    window.Gear(gearsvg, gearobj, "")


    gearobj = 
        teeth:7
        offset:[19*5,0]
        direction:-1
        rotation:3.5
    

    window.Gear(gearsvg, gearobj, "")
    gearobj = 
        teeth:7
        offset:[-19*5,0]
        direction:-1
        rotation:-3.5
    

    window.Gear(gearsvg, gearobj, "")
    div
# //END GENERATE GOLD GEAR ACCENT