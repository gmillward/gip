import numpy as np
import matplotlib.pyplot as plt

data = np.loadtxt('profile_mp7_lp49.dat')
alt   = data[:, 0]
glat  = data[:, 1]
oplus = data[:, 2]
idx   = np.arange(len(alt))

fig, axes = plt.subplots(1, 3, figsize=(18, 6))
fig.suptitle('O+ density profile  —  mp=7, lp=49  (UT=12:15, LT=09:09)', fontsize=13)

# --- Left: O+ vs altitude ---
ax = axes[0]
ax.semilogy(oplus, alt, 'b.-', markersize=4, linewidth=1)
ax.set_ylabel('Altitude (km)')
ax.set_xlabel('O+ density (m$^{-3}$)')
ax.set_title('O+ vs Altitude')
ax.grid(True, which='both', alpha=0.3)

# --- Middle: O+ vs geographic latitude ---
ax = axes[1]
ax.semilogy(glat, oplus, 'g.-', markersize=4, linewidth=1)
ax.set_xlabel('Geographic latitude (deg)')
ax.set_ylabel('O+ density (m$^{-3}$)')
ax.set_title('O+ vs Geographic Latitude')
ax.grid(True, which='both', alpha=0.3)
ax.axvline(0, color='k', linestyle='--', linewidth=0.8, alpha=0.5, label='Equator')
ax.legend(fontsize=9)

# --- Right: O+ vs grid index (with alt and lat annotated) ---
ax = axes[2]
ax.semilogy(idx, oplus, 'r.-', markersize=4, linewidth=1)
ax.set_xlabel('Grid index along tube (IN → IS)')
ax.set_ylabel('O+ density (m$^{-3}$)')
ax.set_title('O+ vs Grid Index')
ax.grid(True, which='both', alpha=0.3)

# Annotate a few key points with alt and lat
for ann_i in [0, len(idx)//4, len(idx)//2, 3*len(idx)//4, len(idx)-1]:
    ax.annotate(f'{alt[ann_i]:.1f}km\n{glat[ann_i]:.1f}°',
                xy=(idx[ann_i], oplus[ann_i]),
                fontsize=7, ha='center',
                xytext=(idx[ann_i], oplus[ann_i]*3),
                arrowprops=dict(arrowstyle='->', color='grey', lw=0.8))

plt.tight_layout()
plt.savefig('profile_mp7_lp49.png', dpi=150)
print('Saved profile_mp7_lp49.png')
plt.show()
