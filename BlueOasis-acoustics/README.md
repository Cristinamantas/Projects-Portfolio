# Underwater Acoustic Analysis – Internship Project (BlueOasis)

This project was developed as part of my internship with **BlueOasis**, where I contributed to the analysis of underwater acoustic data using Python.

The code performs calibrated spectral and statistical analysis of underwater `.wav` files, including:
- Pressure conversion from raw audio using calibration constants
- Spectrogram generation (PSD) with custom time and dB scaling
- PamGuide-style third-octave band analysis with RMS and percentile overlays
- SPL and TOL visualizations based on calibrated power spectra

The analysis helps visualize and interpret ambient underwater noise conditions using reproducible signal processing techniques.

Libraries used:  
`NumPy`, `SciPy`, `Matplotlib`, `datetime`, and `os`

> This tool was applied to real-world hydrophone recordings collected during oceanographic monitoring.
