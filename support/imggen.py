import sys, os
sys.path = ['/usr/lib/gimp/2.0/python'] + sys.path
from gimpfu import *

COLORS = ['000000','0000ff','00ff00','27249c','54fa0d','5c5c5c', '775acf','7f7d0a',
          '808080','899675','98d5e4','b5b5b5', 'cd1f3c','f62db6','ff0000','ffffff']
MODES = [NORMAL_MODE, DISSOLVE_MODE, MULTIPLY_MODE, SCREEN_MODE, DIFFERENCE_MODE,
         ADDITION_MODE, SUBTRACT_MODE, DARKEN_ONLY_MODE, LIGHTEN_ONLY_MODE, HUE_MODE,
         SATURATION_MODE, COLOR_MODE, VALUE_MODE, DIVIDE_MODE, DODGE_MODE,
         BURN_MODE, HARDLIGHT_MODE, SOFTLIGHT_MODE, GRAIN_EXTRACT_MODE, GRAIN_MERGE_MODE]
MODE_LABELS = {
    NORMAL_MODE: 'normal', DISSOLVE_MODE: 'dissolve', MULTIPLY_MODE: 'multiply',
    SCREEN_MODE: 'screen', DIFFERENCE_MODE: 'difference', ADDITION_MODE: 'addition',
    SUBTRACT_MODE: 'subtract', DARKEN_ONLY_MODE: 'darken_only', LIGHTEN_ONLY_MODE: 'lighten_only',
    HUE_MODE: 'hue', SATURATION_MODE: 'saturation', COLOR_MODE: 'color',
    VALUE_MODE: 'value', DIVIDE_MODE: 'divide', DODGE_MODE: 'dodge',
    BURN_MODE: 'burn', HARDLIGHT_MODE: 'hardlight', SOFTLIGHT_MODE: 'softlight',
    GRAIN_EXTRACT_MODE: 'grain_extract', GRAIN_MERGE_MODE: 'grain_merge'
}
ALPHA_VALUES = [20.0, 43.0, 60.0, 78.0, 100.0]


def mk_img_path(color, mode, alpha):
    cur = os.path.abspath(os.curdir)
    dir = os.path.join(cur, os.path.join('ref', os.path.join(color, os.path.join(MODE_LABELS[mode], str(alpha/100)))))
    if not os.path.exists(dir):
        os.makedirs(dir)
    return os.path.join(dir, 'colors.png')

def load_base(img):
    cur = os.path.abspath(os.curdir)
    img.remove_layer(img.layers[0])
    base = pdb.gimp_file_load_layer(img, os.path.join(cur, 'base.png'))
    img.add_layer(base, 0)

def mk_color_layer(img, color, mode, alpha):
    w = img.width
    h = img.height
    layer = gimp.Layer(img, "Mask", w, h, RGBA_IMAGE, alpha, mode)
    img.add_layer(layer, -1)
    gimp.set_foreground('#' + color)
    layer.fill(FG_BUCKET_FILL)
    return img.merge_down(layer, CLIP_TO_IMAGE)

def create_ref_image(img, color, mode, alpha):
    color_layer = mk_color_layer(img, color, mode, alpha) 
    path = mk_img_path(color, mode, alpha)
    pdb.file_png_save_defaults(img, color_layer, path, 'colors.png')

def create_ref_images(img):
    for c in COLORS:
        for m in MODES:
            for a in ALPHA_VALUES:
                load_base(img)
                create_ref_image(img, c, m, a)
