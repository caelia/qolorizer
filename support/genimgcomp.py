#!/usr/bin/env python

mode_group_header = '''(test-group "[%(mode_idx)d] %(mode_name)s"'''

color_group_header = '''  (test-group "[%(mode_idx)d.%(color_idx)02d] %(mode_name)s/%(color_code)s"'''

body_tpl = '''    (test
      "%(mode_idx)d.%(color_idx)02d.%(alpha_idx)02d: %(mode_name)s/%(color_code)s/%(alpha_level)s"
      0
      (compare-image '%(mode_name)s "%(color_code)s" "%(alpha_level)s"))'''

testable_blend_modes = (
    "addition", "burn", "color", "darken-only", "difference", "divide", "dodge",
    "grain-extract", "grain-merge", "hard-light", "hue", "lighten-only", "multiply",
    "normal", "saturation", "screen", "subtract", "value"
)

colors = ("000000", "0000ff", "00ff00", "27249c", "54fa0d", "5c5c5c", "775acf", "7f7d0a",
          "808080", "899675", "98d5e4", "b5b5b5", "cd1f3c", "f62db6", "ff0000", "ffffff")

alpha_levels = ("0.2", "0.43", "0.6", "0.78", "1.0")

for i in range(len(testable_blend_modes)):
    print(mode_group_header % {'mode_idx': (i+1), 'mode_name': testable_blend_modes[i]})
    for j in range(len(colors)):
        print(color_group_header % 
              {'mode_idx': (i+1), 'mode_name': testable_blend_modes[i],
               'color_idx': (j+1), 'color_code': colors[j]})
        for k in range(len(alpha_levels)):
            print(body_tpl %
                  {'mode_idx': (i+1), 'mode_name': testable_blend_modes[i],
                   'color_idx': (j+1), 'color_code': colors[j],
                   'alpha_idx': (k+1), 'alpha_level': alpha_levels[k]})
        print('  )')
    print(')')

