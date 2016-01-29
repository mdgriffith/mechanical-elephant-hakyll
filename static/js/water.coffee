
Coords =
  createGrid: (width, height, resolution, variance) ->
    offset = (-1*resolution) - variance

    # create grid of vertices
    vertices = []
    for x in [0 .. Math.floor(width/resolution)+3]
      row = []
      for y in [0 .. Math.floor(height/resolution)+3]
        row.push new Two.Vector (x*resolution)+(variance*Math.random())+offset, (y*resolution)+(variance*Math.random())+offset
      vertices.push row
    return vertices

  transpose: (array, arrayLength) ->
    newArray = []
    for i in [0..array.length-1]
      newArray.push []

    for row in array
      for value, j in row
        newArray[j].push(value)
    return newArray

  average: (vectors) ->
    total = [0,0]
    for v in vectors
      total[0] += v.x
      total[1] += v.y
    return {'x':total[0]/3, 'y':total[1]/3}

  toQuadCoords: (xy) ->
    coords = []
    if xy.x - 1 >= 0
      if xy.y - 1 >= 0
        coords.push [xy.x - 1, xy.y - 1]
        coords.push [xy.x , xy.y - 1]
        coords.push [xy.x - 1, xy.y ]
        coords.push [xy.x, xy.y]
      else
        coords.push [xy.x - 1, xy.y ]
        coords.push [xy.x, xy.y]
    else
      if xy.y - 1 >= 0
        coords.push [xy.x , xy.y - 1]
        coords.push [xy.x, xy.y]
      else
        coords.push [xy.x, xy.y]
    return coords

  quadToTriangleTopLeft: (quad) ->
      return new Two.Polygon([quad[0], quad[1], quad[2]], true, false, false);

  quadToTriangleBottomRight: (quad) ->
      return new Two.Polygon([quad[1], quad[2], quad[3]], true, false, false);

  quadToTriangleTopRight: (quad) ->
      return new Two.Polygon([quad[0], quad[1], quad[3]], true, false, false);

  quadToTriangleBottomLeft: (quad) ->
      return new Two.Polygon([quad[0], quad[2], quad[3]], true, false, false);






mixColorsByDistance = (vector, colors) ->

  distances = []
  for color in colors
    x = color.x * two.width
    y = color.y * two.height

    dx = (Math.abs(x - vector.x)/two.width)
    dy = (Math.abs(y - vector.y)/two.height)
    distance = Math.sqrt (dx*dx + dy*dy)
    distances.push [distance, color]

  sumDist = 0
  for d in distances
    sumDist += d[0]
  rgb = [0,0,0]
  for d in distances
    factor = 1 - (d[0]/sumDist)
    c = d[1].rgb
    rgb[0] = rgb[0] + (factor * c[0])
    rgb[1] = rgb[1] + (factor * c[1])
    rgb[2] = rgb[2] + (factor * c[2])

  # console.log rgb
  return "rgb(#{parseInt(rgb[0])},#{parseInt(rgb[1])},#{parseInt(rgb[2])})"




Color =
  mixByVector: (vector, colors, base) ->
    # rgb = base

    totalFactor = 0
    factorColors = []
    for color in colors
      x = color.x
      y = color.y

      dx = Math.abs(x - vector.x)
      dy = Math.abs(y - vector.y)
      distance = Math.sqrt (dx*dx + dy*dy)
      factor = color.factor distance
      if factor < 0
        factor = 0
      totalFactor += factor
      factorColors.push {'factor':factor, 'color':color.rgb}

    if totalFactor < 1
      factorColors.push {'factor':1-totalFactor, 'color':base}
      totalFactor = 1

    rgb = [0,0,0]
    for fColor in factorColors
      normalized = fColor.factor/totalFactor
      if normalized == 0
        continue
      rgb = Color.mix rgb, fColor.color, normalized

    Color.vary rgb, 15
    return "rgb(#{parseInt(rgb[0])},#{parseInt(rgb[1])},#{parseInt(rgb[2])})"

  mix: (rgb1, rgb2, factor) ->
    rgb = []
    rgb.push (rgb1[0]*(1-factor)) + (rgb2[0]*factor)
    rgb.push (rgb1[1]*(1-factor)) + (rgb2[1]*factor)
    rgb.push (rgb1[2]*(1-factor)) + (rgb2[2]*factor)
    return rgb

  vary: (rgb, variance) ->
    var1 = Math.random()*variance

    rgb[0] += var1
    rgb[1] += var1
    rgb[3] += var1



class ColorPoint

  constructor: (relx, rely, {rgb, falloff_radius, falloff_easing}) ->
    @relx = relx
    @rely = rely
    @x = null
    @y = null
    @rgb = rgb
    @falloff_radius = falloff_radius
    @falloff_easing = falloff_easing

  setXY: (x, y) ->
    @x = x
    @y = y

  factor: (distance) ->
    normalizedDistance = distance / @falloff_radius
    if @falloff_easing
      return 1 - @falloff_easing normalizedDistance
    else
      return 1 - normalizedDistance



Easing =
  linear: (d) -> d
  square: (d) -> d * d
  cubic: (d) -> d * d * d
  flat: (percent) ->
      easing = (d) ->
        if d >= percent
          return d-percent
        else
          return 0
      return easing

Draw =
  circles: (two, vertices) ->
    for row in vertices
      for xy in row
        circle = two.makeCircle(xy.x, xy.y, 4)
        circle.fill = '#000000'

  triangles: (two, vertices, colors, base) ->
    quads = {}
    # prepare quad list
    # quads[x][y] = [xy1, xy2, xy3, xy4]
    for row, x in vertices
      for xy, y in row
        qcoords = Coords.toQuadCoords {'x':x,'y':y}
        # console.log qcoords
        for q in qcoords
          if q[0] of quads
            if q[1] of quads[q[0]]
              quads[q[0]][q[1]].push xy
            else
              quads[q[0]][q[1]] = [xy]
          else
            quads[q[0]] = {}
            quads[q[0]][q[1]] = [xy]

    triangles = []
    rowlength = vertices.length-1
    for qx in [0 .. rowlength]
      row = []
      columnlength = vertices[0].length-1
      for qy in [0 .. columnlength]
        quad = quads[qx][qy]
        if quad.length != 4
          continue

        if (rowlength/2 <= qx and columnlength/2 <= qy) or (rowlength/2 > qx and columnlength/2 > qy)
          tri = Coords.quadToTriangleTopLeft(quad)
          c1 = Color.mixByVector Coords.average(tri.vertices), colors, base
          tri.fill = c1
          tri.stroke = c1
          row.push(tri)
          two.add(tri)

          tri2 = Coords.quadToTriangleBottomRight(quad)
          c2 = Color.mixByVector Coords.average(tri2.vertices), colors, base
          tri2.fill = c2
          tri2.stroke = c2
          row.push(tri2)
          two.add(tri2)
        else
          tri = Coords.quadToTriangleTopRight(quad)
          c1 = Color.mixByVector Coords.average(tri.vertices), colors, base
          tri.fill = c1
          tri.stroke = c1
          row.push(tri)
          two.add(tri)

          tri2 = Coords.quadToTriangleBottomLeft(quad)
          c2 = Color.mixByVector Coords.average(tri2.vertices), colors, base
          tri2.fill = c2
          tri2.stroke = c2
          row.push(tri2)
          two.add(tri2)

      triangles.push row
    return triangles

  waves: (two, vectors, colors) ->
    seaweed = []

    t = Coords.transpose vectors
    for row in t
      row.push new Two.Vector two.width, two.height + 100
      row.push new Two.Vector 0, two.height + 100
      tri = new Two.Polygon(row, false, true, false)
      c1 = Color.mixByVector {'x':two.height/2, 'y':row[0].y}, colors
      tri.fill = c1
      tri.stroke =  c1
      two.add(tri)
      seaweed.push(tri)


    # for row in vectors
    #   # row.push Two.Vector two.width, two.height
    #   # row.push Two.Vector 0, two.height
    #   tri = new Two.Polygon(row, false, true, false)
    #   # c1 = mixColors {'x':row[0].x, 'y':row[0].y}, colors
    #   tri.fill = "rgb(0,200,0)"
    #   tri.stroke =  "rgb(30,180,30)"
    #   two.add(tri)
    #   seaweed.push(tri)

    # seaweed = []


    return seaweed



class PolyBackground

  constructor: (elem, resolution, variance, colors, bgColor) ->
    @resolution = resolution
    @variance = variance
    @colors = colors
    @vectors = null
    @triangles = null
    @bgColor = bgColor
    @elem = elem
    @two = new Two({
        'type': Two.Types.canvas
        # width:@elem.offsetWidth
        # height:@elem.offsetHeight
        # fullscreen: true
     }).appendTo(elem)

    @bind()
    @resize = ->

  bind: () ->
    drawWithcontext = () => @draw()
    # window.onresize = _.debounce(drawWithcontext, 1500)

  onResize: (cb) ->
    @resize = cb.bind(this)

  draw: () ->
    @two.clear()
    @two.width = @elem.offsetWidth
    @two.height = @elem.offsetHeight
    @resize()
    # for c in @colors
      # c.calcXY @two.width, @two.height

    if @vectors isnt null
      @vectors = null
      @triangles = null
    @vectors = Coords.createGrid @two.width, @two.height, @resolution, @variance
    @triangles = Draw.triangles @two, @vectors, @colors, @bgColor
    @two.update()

    

  animate: (delta, cyclePerMS) ->
    # timeFactor = 0.0005
    #
    totalT = 0

    # Math.PI / 2
    # prevDelta = 0

    anim = @two.bind 'update', (frameCount, dt) =>
      # This code is called everytime two.update() is called.
      # Effectively 60 times per second.
      totalT += dt

      cycles = (totalT/cyclePerMS)*(Math.PI/2)

    
      columnLength = @vectors.length
      for row, x in @vectors
        rowLength = row.length
        for xy, y in row
          if x == 0 or y == 0 or x == columnLength-1 or y == rowLength-1
            continue
          # console.log (Math.sin((totalT*timeFactor) + x/10))
          if 'prev' of xy
            prevDelta = xy.prev
          else
            prevDelta = 0

          newDelta = (delta * Math.sin(cycles + x))

          newY = xy.y + newDelta - prevDelta
          xy.prev = newDelta
          xy.set xy.x, newY

      # prevDelta = delta * Math.sin(cycles)

      # console.log "set colors"
      # for row, x in @triangles
      #   for tri, y in row
      #     color = mixColors average(tri.vertices), colors
      #     # console.log "set color to #{color}"
      #     tri.fill = color
      #     tri.stroke = color
    anim.play();  # Finally, start the animation loop

# drawBackground()
# window.onresize = -> drawBackground()

window.ColorPoint = ColorPoint
window.Easing = Easing
window.PolyBackground = PolyBackground
