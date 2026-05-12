#!/usr/bin/env python3
"""
Plot wall-clock timings of GT and GIP loops from a gt_gip_model log file.

Usage:
    python plot_timings.py [logfile]

Default logfile: /tmp/gt_gip_run5.log
Works on a partial (still-running) log.
"""

import os
import sys
import re
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

NNSTOP = 1440   # steps per day (from input_equinox_apex3)

def parse_log(path):
    gt_x, gt_t = [], []
    gip_x, gip_t = [], []
    current_day = 1

    re_day  = re.compile(r'DAY NUMBER\s+(\d+)')
    re_gt   = re.compile(r'TIMING GT\s+nnloop=\s*(\d+)\s+wall=\s*([\d.]+)')
    re_gip  = re.compile(r'TIMING GIP\s+nnloop=\s*(\d+)\s+wall=\s*([\d.]+)')

    with open(path) as f:
        for line in f:
            m = re_day.search(line)
            if m:
                current_day = int(m.group(1))
                continue

            m = re_gt.search(line)
            if m:
                nnloop = int(m.group(1))
                wall   = float(m.group(2))
                x = (current_day - 1) + (nnloop - 1) / NNSTOP
                gt_x.append(x)
                gt_t.append(wall)
                continue

            m = re_gip.search(line)
            if m:
                nnloop = int(m.group(1))
                wall   = float(m.group(2))
                x = (current_day - 1) + (nnloop - 1) / NNSTOP
                gip_x.append(x)
                gip_t.append(wall)

    return gt_x, gt_t, gip_x, gip_t


def main():
    logfile = sys.argv[1] if len(sys.argv) > 1 else '/tmp/gt_gip_run5.log'

    print(f'Reading {logfile} ...')
    gt_x, gt_t, gip_x, gip_t = parse_log(logfile)
    print(f'  GT points : {len(gt_x)}')
    print(f'  GIP points: {len(gip_x)}')

    if not gt_x and not gip_x:
        print('No timing data found.')
        return

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 7), sharex=True)
    fig.suptitle('GT-GIP wall-clock timing per timestep', fontsize=13)

    if gt_x:
        ax1.plot(gt_x, gt_t, color='steelblue', linewidth=0.8, label='GT wall time')
        ax1.set_ylabel('Wall time (s)')
        ax1.set_title('GT thermosphere')
        ax1.legend(loc='upper left')
        ax1.grid(True, alpha=0.3)
        ax1.yaxis.set_minor_locator(ticker.AutoMinorLocator())

    if gip_x:
        ax2.plot(gip_x, gip_t, color='darkorange', linewidth=0.8, label='GIP wall time')
        ax2.set_ylabel('Wall time (s)')
        ax2.set_title('GIP ionosphere-plasmasphere')
        ax2.legend(loc='upper left')
        ax2.grid(True, alpha=0.3)
        ax2.yaxis.set_minor_locator(ticker.AutoMinorLocator())

    ax2.set_xlabel('Simulation day')
    ax2.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
    ax2.xaxis.set_minor_locator(ticker.AutoMinorLocator())

    plt.tight_layout()
    logname = os.path.splitext(os.path.basename(logfile))[0]
    outfile = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'graphics',
                           f'{logname}_timings.png')
    plt.savefig(outfile, dpi=150)
    print(f'Saved {outfile}')
    plt.show()


if __name__ == '__main__':
    main()
