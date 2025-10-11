#!/bin/bash
# install-fonts.sh - Download and install required fonts for Emacs configuration

set -e  # Exit on error

# Color codes for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Detect OS and set font directory
detect_os() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        OS="linux"
        FONT_DIR="$HOME/.local/share/fonts"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        OS="macos"
        FONT_DIR="$HOME/Library/Fonts"
    elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "win32" ]]; then
        OS="windows"
        FONT_DIR="$LOCALAPPDATA/Microsoft/Windows/Fonts"
    else
        OS="unknown"
        FONT_DIR="$HOME/.fonts"
    fi
}

# Create necessary directories
setup_directories() {
    echo -e "${BLUE}Setting up directories...${NC}"
    mkdir -p "$FONT_DIR"
    mkdir -p "$TEMP_DIR"
    echo -e "${GREEN}✓${NC} Font directory: $FONT_DIR"
    echo -e "${GREEN}✓${NC} Temp directory: $TEMP_DIR"
}

# Download a file
download_file() {
    local url="$1"
    local output="$2"
    echo -e "${BLUE}Downloading:${NC} $(basename "$output")"
    
    if command -v curl &> /dev/null; then
        curl -L -o "$output" "$url" --progress-bar
    elif command -v wget &> /dev/null; then
        wget -O "$output" "$url" -q --show-progress
    else
        echo -e "${RED}✗ Neither curl nor wget found. Please install one.${NC}"
        exit 1
    fi
}

# Extract zip file
extract_zip() {
    local zip_file="$1"
    local dest_dir="$2"
    
    echo -e "${BLUE}Extracting:${NC} $(basename "$zip_file")"
    
    if command -v unzip &> /dev/null; then
        unzip -o -q "$zip_file" -d "$dest_dir"
    elif command -v 7z &> /dev/null; then
        7z x -y -o"$dest_dir" "$zip_file" > /dev/null
    else
        echo -e "${RED}✗ No unzip utility found. Please install unzip or 7z.${NC}"
        exit 1
    fi
}

# Install Source Code Pro
install_source_code_pro() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Installing: Source Code Pro${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    local url="https://github.com/adobe-fonts/source-code-pro/releases/download/2.042R-u%2F1.062R-i%2F1.026R-vf/OTF-source-code-pro-2.042R-u_1.062R-i.zip"
    local zip_file="$TEMP_DIR/source-code-pro.zip"
    local extract_dir="$TEMP_DIR/source-code-pro"
    
    download_file "$url" "$zip_file"
    mkdir -p "$extract_dir"
    extract_zip "$zip_file" "$extract_dir"
    
    local count=0
    # Install all font files recursively (ttf, otf, woff, woff2)
    while IFS= read -r -d '' font; do
        cp "$font" "$FONT_DIR/"
        echo -e "  ${GREEN}✓${NC} Installed: $(basename "$font")"
        ((count++))
    done < <(find "$extract_dir" -type f \( -name "*.ttf" -o -name "*.otf" -o -name "*.woff" -o -name "*.woff2" \) -print0)
    
    echo -e "${GREEN}✓ Source Code Pro installed ($count files)${NC}"
}

# Install Nerd Fonts (Source Code Pro variant)
install_nerd_fonts() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Installing: Source Code Pro Nerd Font${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    # Note: Use latest stable release from https://github.com/ryanoasis/nerd-fonts/releases
    local url="https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/SourceCodePro.zip"
    local zip_file="$TEMP_DIR/nerd-fonts.zip"
    local extract_dir="$TEMP_DIR/nerd-fonts"
    
    download_file "$url" "$zip_file"
    mkdir -p "$extract_dir"
    extract_zip "$zip_file" "$extract_dir"
    
    local count=0
    # Install all font files recursively (ttf, otf, woff, woff2)
    while IFS= read -r -d '' font; do
        cp "$font" "$FONT_DIR/"
        echo -e "  ${GREEN}✓${NC} Installed: $(basename "$font")"
        ((count++))
    done < <(find "$extract_dir" -type f \( -name "*.ttf" -o -name "*.otf" -o -name "*.woff" -o -name "*.woff2" \) -print0)
    
    echo -e "${GREEN}✓ Nerd Fonts installed ($count files)${NC}"
}

# Install Droid Sans Fallback
install_droid_sans() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Installing: Droid Sans Fallback${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    local url="https://github.com/LibreOffice/core/raw/master/extras/source/truetype/symbol/DroidSansFallback.ttf"
    local font_file="$FONT_DIR/DroidSansFallback.ttf"
    
    download_file "$url" "$font_file"
    echo -e "${GREEN}✓ Droid Sans Fallback installed${NC}"
}

# Install Source Han Sans CN
install_source_han_sans() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Installing: Source Han Sans CN${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    local url="https://github.com/adobe-fonts/source-han-sans/releases/download/2.004R/SourceHanSansCN.zip"
    local zip_file="$TEMP_DIR/source-han-sans.zip"
    local extract_dir="$TEMP_DIR/source-han-sans"
    
    download_file "$url" "$zip_file"
    mkdir -p "$extract_dir"
    extract_zip "$zip_file" "$extract_dir"
    
    local count=0
    # Install all font files recursively (ttf, otf, woff, woff2)
    while IFS= read -r -d '' font; do
        cp "$font" "$FONT_DIR/"
        echo -e "  ${GREEN}✓${NC} Installed: $(basename "$font")"
        ((count++))
    done < <(find "$extract_dir" -type f \( -name "*.ttf" -o -name "*.otf" -o -name "*.woff" -o -name "*.woff2" \) -print0)
    
    echo -e "${GREEN}✓ Source Han Sans CN installed ($count files)${NC}"
}

# Refresh font cache
refresh_font_cache() {
    echo -e "\n${BLUE}Refreshing font cache...${NC}"
    
    if [[ "$OS" == "linux" ]]; then
        if command -v fc-cache &> /dev/null; then
            echo -e "${BLUE}Running: fc-cache -fv ${FONT_DIR}${NC}"
            fc-cache -fv "${FONT_DIR}"
            echo -e "${GREEN}✓ Font cache refreshed${NC}"
            
            # Verify fonts are visible
            echo -e "\n${BLUE}Verifying installed fonts...${NC}"
            if fc-list | grep -qi "Source Code Pro"; then
                echo -e "${GREEN}✓ Source Code Pro is visible to fc-list${NC}"
            else
                echo -e "${YELLOW}⚠ Source Code Pro not found in fc-list${NC}"
                echo -e "${YELLOW}  Try: fc-cache -fv && fc-list | grep -i 'source'${NC}"
            fi
        else
            echo -e "${YELLOW}! fc-cache not found. Fonts may not be available until next reboot.${NC}"
        fi
    elif [[ "$OS" == "macos" ]]; then
        echo -e "${GREEN}✓ macOS will refresh fonts automatically${NC}"
    elif [[ "$OS" == "windows" ]]; then
        echo -e "${GREEN}✓ Windows will refresh fonts automatically${NC}"
    fi
}

# Clean up temporary files
cleanup() {
    echo -e "\n${BLUE}Cleaning up...${NC}"
    rm -rf "$TEMP_DIR"
    echo -e "${GREEN}✓ Temporary files removed${NC}"
}

# Main installation function
main() {
    echo -e "\n${BLUE}═════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  Emacs Font Installation Script${NC}"
    echo -e "${BLUE}═════════════════════════════════════════════════════${NC}\n"
    
    # Detect OS
    detect_os
    echo -e "${GREEN}Detected OS:${NC} $OS"
    
    # Set up temporary directory
    TEMP_DIR="$HOME/.emacs.d/fonts-download"
    
    # Set up directories
    setup_directories
    
    # Install fonts
    install_source_code_pro
    install_nerd_fonts
    install_droid_sans
    install_source_han_sans
    
    # Refresh font cache
    refresh_font_cache
    
    # Summary
    echo -e "\n${BLUE}═════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  Installation Summary${NC}"
    echo -e "${BLUE}═════════════════════════════════════════════════════${NC}\n"
    echo -e "${GREEN}✓ All fonts installed successfully!${NC}"
    echo -e "${GREEN}Install directory:${NC} $FONT_DIR\n"
    
    # Ask about cleanup
    read -p "Clean up downloaded files? [y/N] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        cleanup
    fi
    
    echo -e "\n${YELLOW}Please restart Emacs (and possibly log out/in) to use the new fonts.${NC}\n"
}

# Run main function
main
