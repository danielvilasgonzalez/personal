#python -m streamlit run ~/Documents/GitHub/personal/r_to_python_app.py

import streamlit as st
import os
import subprocess

def translate_code_ollama(code, direction):
    if direction == "R → Python":
        prompt = f"Translate the following R code to Python using pandas and matplotlib or seaborn:\n\n{code}"
    else:
        prompt = f"Translate the following Python code to R using tidyverse and ggplot2:\n\n{code}"
    
    cmd = ["ollama", "run", "gemma3n"]
    process = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    out, err = process.communicate(prompt)
    if err:
        # Sometimes stderr includes warnings, you can log them if you want
        pass
    return out.strip()

# UI
st.title("🔁 R ↔ Python Translator")

direction = st.radio("Select translation direction:", ["R → Python", "Python → R"])
code_input = st.text_area("Paste your code below:", height=250)

uploaded_file = st.file_uploader("...or upload a .R or .py file", type=["r", "py"])

if uploaded_file is not None:
    file_code = uploaded_file.read().decode("utf-8")
    code_input = file_code
    st.text_area("Uploaded code:", value=file_code, height=250, disabled=True)

if st.button("Translate"):
    if code_input.strip() == "":
        st.warning("Please enter code to translate.")
    else:
        with st.spinner("Translating..."):
            translated_code = translate_code_ollama(code_input, direction)
            st.code(translated_code, language='python' if direction == "R → Python" else 'r')
