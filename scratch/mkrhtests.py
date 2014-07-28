#!/usr/bin/env python

rgb2hsv_tpl = '''      (test
        "5.01.%(n)02d: (rgb>hsv %(r)s %(g)s %(b)s)"
        '(%(h)d %(s)s %(v)s)
        (collect-values (rgb>hsv %(r)s %(g)s %(b)s)))
'''
hsv2rgb_tpl = '''      (test
        "5.02.%(n)02d: (hsv>rgb %(h)d %(s)s %(v)s)"
        '(%(r)s %(g)s %(b)s)
        (collect-values (hsv>rgb %(h)d %(s)s %(v)s)))
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

data = [
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

def rescale_c(n):
    return ('%0.3f' % (n / 255))

def rescale_rgb(rgbi):
    rgbo = []
    for i in range(len(rgbi)):
        rgbo.append(rescale_c(rgbi[i]))
    return rgbo

def rescale_sv(x):
    return ('%0.3f' % (x / 100))

def rescale_hsv(hsvi):
    hsvo = [hsvi[0]]
    hsvo.append(rescale_sv(hsvi[1]))
    hsvo.append(rescale_sv(hsvi[2]))
    return hsvo

for i in range(len(data)):
    rgb, hsv = data[i]
    r, g, b = rescale_rgb(rgb)
    h, s, v = rescale_hsv(hsv)
    print(rgb2hsv_tpl % {'n': i+1, 'r': r, 'g': g, 'b': b, 'h': h, 's': s, 'v': v})

for i in range(len(data)):
    rgb, hsv = data[i]
    r, g, b = rescale_rgb(rgb)
    h, s, v = rescale_hsv(hsv)
    print(hsv2rgb_tpl % {'n': i+1, 'r': r, 'g': g, 'b': b, 'h': h, 's': s, 'v': v})

for i in range(len(data)):
    rgb, hsv = data[i]
    r, g, b = rescale_rgb(rgb)
    print(rgb2hsv2rgb_tpl % {'n': i+1, 'r': r, 'g': g, 'b': b})

for i in range(len(data)):
    rgb, hsv = data[i]
    h, s, v = rescale_hsv(hsv)
    print(hsv2rgb2hsv_tpl % {'n': i+1, 'h': h, 's': s, 'v': v})
