#python -m streamlit run ~/Documents/GitHub/personal/chat_gemma3n_app.py
import streamlit as st
import subprocess

# Title
st.title("💬 Chat with Gemma3n (Local LLM)")

# Function to call the local LLM via Ollama
def chat_with_gemma3n(prompt):
    try:
        cmd = ["ollama", "run", "gemma3n"]
        process = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        out, err = process.communicate(prompt)

        # Optionally show error (toggle this with show_error)
        show_error = False
        if err and show_error:
            st.error(f"Error: {err.strip()}")

        return out.strip()
    except Exception as e:
        st.error(f"Unexpected error: {e}")
        return "Error communicating with gemma3n."

# Session state for conversation history
if "history" not in st.session_state:
    st.session_state.history = []

# Add message to history
def add_message(role, message):
    st.session_state.history.append({"role": role, "message": message})

# Chat input form
with st.form("chat_form", clear_on_submit=True):
    user_input = st.text_area("Your message:", height=100)
    submitted = st.form_submit_button("Send")

# On message submission
if submitted and user_input.strip():
    add_message("user", user_input)

    # Create prompt with full user/assistant history
    prompt = ""
    for m in st.session_state.history:
        if m['role'] == "user":
            prompt += f"user: {m['message']}\n"
        elif m['role'] == "assistant":
            prompt += f"assistant: {m['message']}\n"
    prompt += f"user: {user_input}\nassistant:"

    response = chat_with_gemma3n(prompt)
    add_message("assistant", response)

# Display conversation
for chat in st.session_state.history:
    if chat["role"] == "user":
        st.markdown(f"**You:** {chat['message']}")
    else:
        st.markdown(f"**Gemma3n:** {chat['message']}")
