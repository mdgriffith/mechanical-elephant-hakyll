
rotate = (x, y, xm, ym, a) ->
            a = a * Math.PI / 180 # Convert to radians 

            # Subtract midpoints, so that midpoint is translated to origin
            # and add it in the end again
            xr = (x - xm) * Math.cos(a) - (y - ym) * Math.sin(a)   + xm
            yr = (x - xm) * Math.sin(a) + (y - ym) * Math.cos(a)   + ym

            [xr, yr]
                    


gearline = (d) ->
    n = d.teeth
    r2 = Math.abs(d.radius)
    r0 = r2 - d.toothHeight
    r1 = r2 + d.toothHeight
    if d.annulus
      r3 = r0
      r0 = r1
      r1 = r3
      r3 = r2 + d.thickness
    else 
      r3 = d.radius - d.thickness
    da = Math.PI / n
    a0 = -Math.PI / 2 + ((if d.annulus isnt undefined and d.annulus then Math.PI / n else 0))

    # 2*da = The total length for one space and one tooth to occupy, in radians
    # a0 = the current point on the circle, again radians
    # r0 = radius of circle at base of the tooth
    # r2 = radius of the circle at the mid point of the tooth
    # r1 = radius at the top of the tooth

    i = -1
    path = ["M", r0 * Math.cos(a0), ",", r0 * Math.sin(a0)]
    path.push tooth(r0, r1, r2, a0+(i*2*da), da) while ++i < n
    path.push "M0,#{-r3}A#{r3},#{r3} 0 0,0 0,#{r3}A#{r3},#{r3} 0 0,0 0,#{-r3}Z"
    path.join ""

tooth = (r0, r1, r2, a0, da) ->
    path = []
    path.push "A#{r0},#{r0} 0 0,1 #{r0 * Math.cos(a0 += da)},#{r0 * Math.sin(a0)}"
    path.push "L", r2 * Math.cos(a0), ",", r2 * Math.sin(a0)
    path.push "L", r1 * Math.cos(a0 += da / 3), ",", r1 * Math.sin(a0)
    path.push "A", r1, ",", r1, " 0 0,1 ", r1 * Math.cos(a0 += da / 3), ",", r1 * Math.sin(a0)
    path.push "L", r2 * Math.cos(a0 += da / 3), ",", r2 * Math.sin(a0)
    path.push "L", r0 * Math.cos(a0), ",", r0 * Math.sin(a0)  
    path.join ""  

positioning = (d) ->
    transformation = ""
    if d.offset isnt undefined and d.offset isnt null
        transformation = "translate(#{ d.offset[0] },#{ d.offset[1] })" 
    if d.offset isnt undefined and d.offset isnt null and d.rotation isnt undefined and d.rotation isnt null
        transformation = "#{transformation}rotate(#{ d.rotation },#{ d.offset[0] },#{ d.offset[1] })"
    transformation


# Gear = (frame, teeth, offset, direction, rotation, classed, inverted) ->
#     direction = 1 if direction is null
#     frame.append("g").attr("class", classed).datum(
#       offset:offset
#       rotation:rotation
#       teeth: teeth
#       radius: teeth * 5
#       annulus: inverted
#       direction: direction
#       thickness: 20
#       toothHeight: 7
#     ).attr("transform", positioning).append("path").attr "d", gearline

gear = (frame, data, classed) ->
    data["radius"] = data["teeth"]*5
    data["thickness"] = 20
    data["toothHeight"] = 7
    frame.append("g")
         .attr("class", classed)
         .datum(data)
         .attr("transform", positioning)
         .append("path")
         .attr("radius", data["radius"])
         .attr("direction", data["direction"])
         .attr("d", gearline)

    g = 
      frame:frame
      original:data
      offset:(rotation, classed, data) ->
        if rotation isnt null
          new_offset = lock(@original["teeth"], data["teeth"], rotation, @original["annulus"])
          if "offset" of data
            new_offset[0] = new_offset[0]+data["offset"][0]
            new_offset[1] = new_offset[1]+data["offset"][1]
          data["offset"] = new_offset

        gear(@frame, data, classed)
    g







lock = (size, size2, angle, annulus=false) ->
    console.log size + ":" + size2
    r = size*5
    r2 = size2*5
    if annulus 
      distance = r-r2
    else
      distance = r+r2
    coords = rotate(0,distance,0,0,angle)
    coords


Planetary = (frame) ->
    frame.append("g")
       .attr("class", "inset")

    outer = 
      teeth: 37
      annulus: true
      direction: -1
    annulus = gear(frame, outer, "gear annulus")

    sun_data = 
      teeth: 11
      direction: 1
      offset:[22, -60]
      rotation:3
    sun = gear(frame, sun_data, "gear sun")

    annulus.offset(0, "gear planet", {teeth:19, direction:-1})
    annulus.offset(140, "gear planet", {teeth:11, direction:-1, rotation:2})
    annulus.offset(215, "gear planet", {teeth:7, direction:-1, rotation:-2})


Corner = (frame) ->
    frame.append("g")
       .attr("class", "inset")

    base = gear(frame, {teeth:12,direction:1}, "gear")
    base.offset(0, "gear", {teeth:7, direction:-1, rotation:0})
    base.offset(90, "gear", {teeth:7, direction:-1, rotation:-3.5})



masked = (d) ->
  # path = ["M", "0", ",", "0"]
  path = []
  path.push "M 0 0 l 25 0 l 0 25 l -25 0 z"
  # path.push "M 110 110 L 190 110 L 190 190 L 110 190 z"
  # path.push "M 195.7575,217.10417 c 0,-0.7 -0.08,-1.38 -0.18,-2.06 -0.1,-0.68 -0.2,-1.36 -0.2,-2 0,-0.78 0.2,-1.68 1.68,-1.92 0.1,-0.1 0.1,-0.3 0,-0.4 -0.98,0 -2.84,0.18 -2.84,2.52 0,0.68 0.1,1.42 0.16,2.12 0.06,0.72 0.14,1.38 0.14,2 0,0.68 -0.08,1.54 -1.74,1.66 -0.1,0.12 -0.1,0.32 0,0.44 1.66,0.12 1.74,0.98 1.74,1.66 0,0.6 -0.06,1.28 -0.14,1.98 -0.08,0.72 -0.16,1.44 -0.16,2.14 0,2.33999 1.86,2.52 2.84,2.52 0.1,-0.1 0.1,-0.32 0,-0.4 -1.48,-0.24 -1.68,-1.14 -1.68,-1.92 0,-0.66 0.12,-1.32 0.2,-2 0.08,-0.68 0.18,-1.38 0.18,-2.06 0,-1.14 -0.46,-1.84 -1.5,-2.1 l 0,-0.08 c 1.04,-0.26 1.5,-0.96 1.5,-2.1"
  path.push "M 10,10 c 0,-0.7 -0.08,-1.38 -0.18,-2.06 -0.1,-0.68 -0.2,-1.36 -0.2,-2 0,-0.78 0.2,-1.68 1.68,-1.92 0.1,-0.1 0.1,-0.3 0,-0.4 -0.98,0 -2.84,0.18 -2.84,2.52 0,0.68 0.1,1.42 0.16,2.12 0.06,0.72 0.14,1.38 0.14,2 0,0.68 -0.08,1.54 -1.74,1.66 -0.1,0.12 -0.1,0.32 0,0.44 1.66,0.12 1.74,0.98 1.74,1.66 0,0.6 -0.06,1.28 -0.14,1.98 -0.08,0.72 -0.16,1.44 -0.16,2.14 0,2.33999 1.86,2.52 2.84,2.52 0.1,-0.1 0.1,-0.32 0,-0.4 -1.48,-0.24 -1.68,-1.14 -1.68,-1.92 0,-0.66 0.12,-1.32 0.2,-2 0.08,-0.68 0.18,-1.38 0.18,-2.06 0,-1.14 -0.46,-1.84 -1.5,-2.1 l 0,-0.08 c 1.04,-0.26 1.5,-0.96 1.5,-2.1"
  path.join ""

box = (d) ->
  path = []
  path.push "M 0 0 l 25 0 l 0 25 l -25 0 z"
  path.join ""


Back = (frame) ->
  frame.append("g").attr("class", "backmask")
                    .append("path")
                    .attr("d", box)
                    .attr("transform", "scale(1.0)")


Mask = (frame) ->
  # frame.append("g").attr("class", "backmask")
  #                   .append("path")
  #                   .attr("d", box)
  #                   .attr("transform", "scale(5.0)")


  frame.append("g").attr("class", "mask")
                    .append("path")
                    .attr("d", masked)
                    .attr("transform", "scale(1.0)")

 


window.Planetary = Planetary
window.Gear = gear
window.Back = Back
window.Mask = Mask
window.Lock = lock
window.Corner = Corner