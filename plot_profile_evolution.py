import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm

data = np.loadtxt('C:/dev/gt-gip/profiles_mp7_lp49_all.dat',
                  usecols=(0,1,2,3,4))
ut_all    = data[:,0]
lt_all    = data[:,1]
alt_all   = data[:,2]
glat_all  = data[:,3]
oplus_all = data[:,4]

uts = np.unique(ut_all)
n   = len(uts)

# Colour by LT
lts = np.array([lt_all[ut_all == ut][0] for ut in uts])
lt_norm = (lts - lts.min()) / (lts.max() - lts.min())
colors = cm.plasma(lt_norm)

fig, axes = plt.subplots(1, 3, figsize=(18, 7))
fig.suptitle('O+ density evolution — mp=7, lp=49 (all GIP calls, coloured by LT)\n'
             'Dawn terminator crossing visible as profile collapses near LT~06', fontsize=12)

ax_alt  = axes[0]
ax_lat  = axes[1]
ax_idx  = axes[2]

for k, ut in enumerate(uts):
    mask  = ut_all == ut
    alt   = alt_all[mask]
    glat  = glat_all[mask]
    oplus = oplus_all[mask]
    idx   = np.arange(len(alt))
    c     = colors[k]
    lw    = 2.0 if abs(lts[k] - 6.0) < 0.5 else 0.6
    alpha = 1.0 if abs(lts[k] - 6.0) < 0.5 else 0.4

    ax_alt.semilogy(oplus, alt,  color=c, lw=lw, alpha=alpha)
    ax_lat.semilogy(glat,  oplus, color=c, lw=lw, alpha=alpha)
    ax_idx.semilogy(idx,   oplus, color=c, lw=lw, alpha=alpha)

# Highlight the failure-time profiles (LT near 06)
for ax in axes:
    ax.grid(True, which='both', alpha=0.2)

ax_alt.set_xlabel('O+ density (m$^{-3}$)')
ax_alt.set_ylabel('Altitude (km)')
ax_alt.set_title('O+ vs Altitude')

ax_lat.set_xlabel('Geographic latitude (deg)')
ax_lat.set_ylabel('O+ density (m$^{-3}$)')
ax_lat.set_title('O+ vs Latitude')
ax_lat.axvline(0, color='k', lw=0.8, ls='--', alpha=0.5, label='Equator')
ax_lat.legend(fontsize=9)

ax_idx.set_xlabel('Grid index (IN → IS)')
ax_idx.set_ylabel('O+ density (m$^{-3}$)')
ax_idx.set_title('O+ vs Grid Index')

# Colorbar for LT
sm = cm.ScalarMappable(cmap='plasma',
                       norm=plt.Normalize(vmin=lts.min(), vmax=lts.max()))
sm.set_array([])
cbar = fig.colorbar(sm, ax=axes, fraction=0.015, pad=0.02)
cbar.set_label('Local Time (hr)')

plt.savefig('C:/dev/gt-gip/profile_evolution_mp7_lp49.png', dpi=150, bbox_inches='tight')
print('Saved profile_evolution_mp7_lp49.png')
plt.show()
