#!/usr/bin/env python

rgb2hsv_tpl = '''      (test
        "5.01.%(n)02d: (rgb>hsv %(r)s %(g)s %(b)s)"
        '(%(h)d %(s)s %(v)s)
        (collect-values rgb>hsv %(r)s %(g)s %(b)s))
'''
hsv2rgb_tpl = '''      (test
        "5.02.%(n)02d: (hsv>rgb %(h)d %(s)s %(v)s)"
        '(%(r)s %(g)s %(b)s)
        (collect-values hsv>rgb %(h)d %(s)s %(v)s))
'''
rgb2hsv2rgb_tpl = '''      (test
        "5.03.%(n)02d: (hsv>rgb (rgb>hsv %(r)s %(g)s %(b)s))"
        '(%(r)s %(g)s %(b)s)
        (collect-values
          (call-with-values (lambda () (rgb>hsv %(r)s %(g)s %(b)s)) hsv>rgb)))
'''
hsv2rgb2hsv_tpl = '''     (test
        "5.04.%(n)02d: (rgb>hsv (hsv>rgb %(h)d %(s)s %(v)s))"
        '(%(h)d %(s)s %(v)s)
        (collect-values
          (call-with-values (lambda () (hsv>rgb %(h)d %(s)s %(v)s)) rgb>hsv)))
'''
group5_hdr = '''(test-group "[5] RGB <-> HSV conversions"
'''
group501_hdr = '''  (test-group "[5.01] rgb>hsv - match results"
    (with-comparator list-fuzzy=
'''
group502_hdr = '''  (test-group "[5.02] hsv>rgb - match results"
    (with-comparator list-fuzzy=
'''
group503_hdr = '''  (test-group "[5.03] RGB -> HSV -> RGB roundtrip"
    (with-comparator list-fuzzy=
'''
group504_hdr = '''  (test-group "[5.04] HSV -> RGB -> HSV roundtrip"
    (with-comparator list-fuzzy=
'''

bad_data = [
    ((255, 255, 255), (0, 0, 100)),
    ((196, 196, 196), (0, 0, 77)),
    ((133, 133, 133), (0, 0, 52)),
    ((64, 64, 64), (0, 0, 25)),
    ((0, 0, 0), (0, 0, 0)),
    ((255, 0, 0), (0, 100, 100)),
    ((212, 255, 0), (70, 100, 100)),
    ((0, 255, 106), (145, 100, 100)),
    ((0, 94, 255), (218, 100, 100)),
    ((201, 0, 255), (287, 100, 100)),
    ((181, 75, 75), (0, 59, 71)),
    ((187, 219, 37), (71, 83, 86)),
    ((109, 147, 125), (145, 26, 58)),
    ((61, 112, 195), (217, 69, 76)),
    ((193, 20, 236), (288, 92, 93)),
    ((142, 0, 0), (0, 100, 56)),
    ((175, 201, 55), (71, 73, 79)),
    ((0, 276, 96), (145, 100, 89)),
    ((85, 117, 171), (218, 50, 67)),
    ((190, 25, 231), (288, 89, 91))
]

raw_data = [
    ((255, 255, 255), (0.0, 0.0, 1.0)),
    ((255, 0, 0), (0.0, 1.0, 1.0)),
    ((181, 75, 75), (0.0, 0.585635364055634, 0.709803938865662)),
    ((142, 0, 0), (0.0, 1.0, 0.556862771511078)),
    ((196, 196, 196), (0.0, 0.0, 0.768627464771271)),
    ((212, 255, 0), (70.1176452636719, 1.0, 1.0)),
    ((187, 219, 37), (70.5494537353516, 0.831050217151642, 0.858823537826538)),
    ((175, 201, 55), (70.684928894043, 0.726368188858032, 0.788235306739807)),
    ((133, 133, 133), (0.0, 0.0, 0.521568655967712)),
    ((0, 255, 106), (144.941177368164, 1.0, 1.0)),
    ((109, 147, 125), (145.263153076172, 0.258503407239914, 0.576470613479614)),
    ((0, 226, 96), (145.486724853516, 1.0, 0.886274516582489)),
    ((64, 64, 64), (0.0, 0.0, 0.250980406999588)),
    ((0, 94, 255), (217.882354736328, 1.0, 1.0)),
    ((61, 112, 195), (217.164184570312, 0.687179505825043, 0.764705896377563)),
    ((85, 117, 171), (217.674423217773, 0.502923965454102, 0.670588254928589)),
    ((0, 0, 0), (0.0, 0.0, 0.0)),
    ((201, 0, 255), (287.294128417969, 1.0, 1.0)),
    ((193, 20, 236), (288.055541992188, 0.915254235267639, 0.925490200519562)),
    ((190, 25, 231), (288.058258056641, 0.891774892807007, 0.905882358551025))
]

data = None

def round_allh():
    global data
    data = []
    for i in range(len(raw_data)):
        datum = list(raw_data[i]) 
        datum[0] = list(datum[0])
        datum[1] = list(datum[1])
        datum[1][0] = round(datum[1][0])
        data.append(datum)

def rescale_c(n):
    return ('%0.3f' % (n / 255))

def rescale_rgb(rgbi):
    rgbo = []
    for i in range(len(rgbi)):
        rgbo.append(rescale_c(rgbi[i]))
    return rgbo

def old_rescale_sv(x):
    return ('%0.3f' % (x / 100))

def rescale_sv(x):
    return ('%0.3f' % x)

def rescale_hsv(hsvi):
    hsvo = [hsvi[0]]
    hsvo.append(rescale_sv(hsvi[1]))
    hsvo.append(rescale_sv(hsvi[2]))
    return hsvo

round_allh()

print(group5_hdr)
print(group501_hdr)
for i in range(len(data)):
    rgb, hsv = data[i]
    r, g, b = rescale_rgb(rgb)
    h, s, v = rescale_hsv(hsv)
    print(rgb2hsv_tpl % {'n': i+1, 'r': r, 'g': g, 'b': b, 'h': h, 's': s, 'v': v})
print('))\n')

print(group502_hdr)
for i in range(len(data)):
    rgb, hsv = data[i]
    r, g, b = rescale_rgb(rgb)
    h, s, v = rescale_hsv(hsv)
    print(hsv2rgb_tpl % {'n': i+1, 'r': r, 'g': g, 'b': b, 'h': h, 's': s, 'v': v})
print('))\n')

print(group503_hdr)
for i in range(len(data)):
    rgb, hsv = data[i]
    r, g, b = rescale_rgb(rgb)
    print(rgb2hsv2rgb_tpl % {'n': i+1, 'r': r, 'g': g, 'b': b})
print('))\n')

print(group504_hdr)
for i in range(len(data)):
    rgb, hsv = data[i]
    h, s, v = rescale_hsv(hsv)
    print(hsv2rgb2hsv_tpl % {'n': i+1, 'h': h, 's': s, 'v': v})
print('))\n')
print(')\n\n(test-exit)\n')
