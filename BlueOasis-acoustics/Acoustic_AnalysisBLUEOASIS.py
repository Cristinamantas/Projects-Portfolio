"""
Acoustic Signal Analysis Module
--------------------------------
This module was developed during my internship to analyze underwater
hydrophone recordings (.wav). It applies calibration, computes power
spectral density (PSD) spectrograms, and generates PamGuide-style
third-octave band (TOL) plots for both PSD and SPL metrics.

Key features:
- Calibrates raw hydrophone data to Pascals
- Generates time frequency spectrograms with proper scaling
- Computes percentile-based noise statistics (PamGuide-style)
- Outputs comparative SPL and PSD plots for acoustic environments
"""

import os
import numpy as np
import matplotlib.pyplot as plt
import scipy.signal
from scipy.io import wavfile
from datetime import datetime, timedelta

# 1) Calibration & SHIFT constants
# --------------------------------------------------
CALIBRATION_GAIN = 1e-3   # Example: converts raw ADC counts to Pascals
p_ref = 1e-6              # Reference pressure in Pa (1 µPa)
B = 1.5                   # Noise power bandwidth for Hann window

# Main shift to bring raw data (e.g. ~+140 dB) into our display range
SHIFT_DB_MAIN = -140.0
# Extra shift for PSD TOL (pull down peaks); for SPL, we want them higher so set to 0.0
EXTRA_SHIFT_FOR_PSD = -10.0
EXTRA_SHIFT_FOR_SPL = 45.0

# Colors & labels for multiple percentile lines (PamGuide style)
COLORS = ["black", "dimgray", "gray", "lightgray", "pink", "magenta"]
LABELS = ["99%", "95%", "50%", "5%", "1%", "RMS"]

def load_wav_file(filepath):
    """Load a WAV file, convert to float32, and apply a calibration gain to get pressure in Pa."""
    if not os.path.exists(filepath):
        print(f"File not found: {filepath}")
        return None, None
    try:
        sr, y = wavfile.read(filepath)
        if y.ndim > 1:
            y = np.mean(y, axis=1)  # Convert to mono
        # Apply calibration gain instead of normalizing
        y = y.astype(np.float32) * CALIBRATION_GAIN
        return y, sr
    except Exception as e:
        print(f"Error loading {filepath}: {e}")
        return None, None

# --------------------------------------------------
# 2) Spectrogram with SHIFT_DB_MAIN so 0 dB (top) to -100 dB (bottom)
# --------------------------------------------------
def calculate_PSD_spectrogram(audio, sr, filename):
    """Compute and plot the PSD spectrogram with calibration applied, then shift for display."""
    nperseg = 2048
    noverlap = int(nperseg * 0.75)
    window = 'hann'
    
    # Compute spectrogram; scaling='density' returns units ~ Pa^2/Hz if input is in Pa
    f, t, Sxx = scipy.signal.spectrogram(audio, sr,
                                         nperseg=nperseg,
                                         noverlap=noverlap,
                                         window=window,
                                         scaling='density')
    
    # Calibrate: divide by (B * p_ref^2), then convert to dB re 1 µPa^2
    Sxx_cal = np.maximum(Sxx / (B * p_ref**2), 1e-30)
    Sxx_dB_raw = 10 * np.log10(Sxx_cal)
    
    print("Spectrogram RAW dB range:", np.min(Sxx_dB_raw), "to", np.max(Sxx_dB_raw))
    
    # Shift raw dB values into display range
    Sxx_dB_shifted = Sxx_dB_raw + SHIFT_DB_MAIN
    # Clamp to [-100, 0] dB
    Sxx_dB_shifted = np.clip(Sxx_dB_shifted, -100, 0)
    
    # Custom time axis: using your original time stamps
    start_time = datetime.strptime("20:41:00", "%H:%M:%S")
    time_labels = [start_time + timedelta(seconds=s) for s in t]
    time_ticks = [datetime.strptime(tick, "%H:%M:%S") for tick in 
                  ["20:41:15", "20:41:30", "20:41:45", "20:42:00"]]
    time_ticks_shifted = [tt - timedelta(seconds=6) for tt in time_ticks]
    
    fig, ax = plt.subplots(figsize=(10, 6))
    pcm = ax.pcolormesh(time_labels, f, Sxx_dB_shifted,
                        shading='nearest', cmap='jet',
                        vmin=-100, vmax=0)
    cbar = plt.colorbar(pcm)
    cbar.set_ticks(np.arange(0, -101, -10))
    cbar.set_label('Relative PSD [dB] (Shifted)')
    
    ax.set_yscale('log')
    ax.set_ylim([3, sr / 2])
    ax.set_ylabel('Frequency [Hz]', fontsize=12)
    ax.set_xlabel('Time', fontsize=12)
    
    plt.xticks(time_ticks_shifted,
               labels=[tt.strftime('%H:%M:%S') for tt in time_ticks],
               rotation=0, ha='center', fontsize=10)
    plt.title(f'Spectrogram of {filename}', fontsize=14)
    plt.show()
    
    return f, t, Sxx  # Return raw Sxx for TOL processing

# --------------------------------------------------
# 3) PamGuide-style multiple percentile lines for TOL
#    (different extra shift for PSD vs. SPL)
# --------------------------------------------------
def calculate_TOL_pamguide_style(f, Sxx, sr, is_SPL=False):
    """
    Compute and plot PamGuide-style multiple percentile lines (99%, 95%, 50%, 5%, 1%, RMS)
    for third-octave band levels. For each frequency bin, percentiles are computed in the
    linear domain and then converted to dB.
    
    If is_SPL is True, a different extra shift is applied (to move the data up).
    """
    # Calibrate Sxx: convert to Pa^2/Hz
    Sxx_cal = np.maximum(Sxx / (B * p_ref**2), 1e-30)
    # Compute raw dB values from Sxx_cal
    Sxx_dB_raw = 10 * np.log10(Sxx_cal)
    
    # Compute percentiles (in linear domain) across time (axis=1)
    p_linear = np.nanpercentile(Sxx_cal, [99, 95, 50, 5, 1], axis=1)
    # Compute RMS in linear domain
    rms_linear = np.nanmean(Sxx_cal, axis=1)
    # Stack percentiles and RMS: shape (6, freq_bins)
    p_all = np.vstack((p_linear, rms_linear))
    
    # Choose extra shift based on type: for SPL, use a higher shift (i.e. less negative)
    extra_shift = EXTRA_SHIFT_FOR_SPL if is_SPL else EXTRA_SHIFT_FOR_PSD
    
    # We'll now compute each line in dB and shift it.
    freq_bins = Sxx_cal.shape[0]
    lines_dB_shifted = []
    for i in range(p_all.shape[0]):
        line_linear = p_all[i, :]
        line_dB = 10 * np.log10(line_linear)
        line_dB_shifted = line_dB + SHIFT_DB_MAIN + extra_shift
        # Fill any NaNs (forward and backward fill)
        for j in range(1, freq_bins):
            if np.isnan(line_dB_shifted[j]):
                line_dB_shifted[j] = line_dB_shifted[j-1]
        for j in range(freq_bins-2, -1, -1):
            if np.isnan(line_dB_shifted[j]):
                line_dB_shifted[j] = line_dB_shifted[j+1]
        lines_dB_shifted.append(line_dB_shifted)
    
    # Now, set up frequency axis for TOL integration:
    # We'll define 1/3-octave bands based on standard center frequencies
    f_ref = 1000.0
    fc_list = []
    for i in range(-20, 40):
        fc = f_ref * 10**((i - 1) / 10.0)
        if 10 <= fc <= 20000:
            fc_list.append(fc)
    fc_list = np.array(fc_list)
    factor = 10**(1/20.0)
    
    # For each band, integrate the corresponding percentiles.
    # Here we will plot each percentile line after interpolating the raw data.
    fig, ax = plt.subplots(figsize=(10, 6))
    for i, line in enumerate(lines_dB_shifted):
        # Interpolate the line onto a uniform frequency grid from 10 to 20000 Hz
        f_uniform = np.logspace(np.log10(10), np.log10(20000), num=1000)
        # Use linear interpolation in log10-space
        interp_func = np.interp(np.log10(f_uniform), np.log10(f), line)
        label_str = LABELS[i] if i < len(LABELS) else f"Line {i}"
        color_str = COLORS[i] if i < len(COLORS) else "blue"
        ax.plot(f_uniform, interp_func, color=color_str, linewidth=2, label=label_str)
    
    ax.legend(loc='lower left')
    if not is_SPL:
        ax.set_ylim([-160, 0])
        ylabel = "PSD TOL [dB] (Shifted)"
        title = "PSD Third-Octave Band Levels (Integrated)"
    else:
        ax.set_ylim([-70, 0])
        ylabel = "Relative SPL [dB] (Shifted)"
        title = "SPL Third-Octave Band Levels (Integrated)"
    
    ax.set_xscale('log')
    ax.set_xlim([10, 20000])
    ax.set_xlabel("Frequency [Hz]", fontsize=12)
    ax.set_ylabel(ylabel, fontsize=12)
    ax.set_title(title, fontsize=14, fontweight='bold')
    # Adjust subplot to pull the data down if necessary
    fig.subplots_adjust(left=0.15, right=0.95, bottom=0.12, top=0.85)
    plt.show()

def process_file(filepath):
    """Process a single WAV file: spectrogram + multi-line TOL (PamGuide style)."""
    audio, sr = load_wav_file(filepath)
    if audio is not None and sr is not None:
        f, t, Sxx = calculate_PSD_spectrogram(audio, sr, os.path.basename(filepath))
        # PSD TOL: is_SPL False (uses EXTRA_SHIFT_FOR_PSD = -10)
        calculate_TOL_pamguide_style(f, Sxx, sr, is_SPL=False)
        # SPL TOL: is_SPL True (uses EXTRA_SHIFT_FOR_SPL = 0)
        calculate_TOL_pamguide_style(f, Sxx, sr, is_SPL=True)

if __name__ == "__main__":
    input_path = "/Users/cristinamantas/Desktop/WAVEDATA/hydro.241121092419.wav"
    if os.path.isdir(input_path):
        for fname in os.listdir(input_path):
            if fname.lower().endswith('.wav'):
                full_wav = os.path.join(input_path, fname)
                print(f"\nProcessing: {full_wav}")
                process_file(full_wav)
    else:
        print(f"Processing single file: {input_path}")
        process_file(input_path)