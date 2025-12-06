#!/bin/bash
set -e

echo "========================================="
echo "Django LiveView Docs - Build & Deploy"
echo "========================================="

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
REMOTE_USER="debian"
REMOTE_HOST="home.server"
REMOTE_PATH="/home/debian/www/django-liveview-docs/public"
LOCAL_BUILD_PATH="./public/"

# Check if remote directory has correct permissions
if ! ssh "${REMOTE_USER}@${REMOTE_HOST}" "test -w ${REMOTE_PATH}" 2>/dev/null; then
    echo -e "${YELLOW}WARNING: No write permissions on ${REMOTE_PATH}${NC}"
    echo -e "${YELLOW}Please run this command on ${REMOTE_HOST}:${NC}"
    echo -e "${BLUE}  sudo chown -R ${REMOTE_USER}:${REMOTE_USER} ${REMOTE_PATH}${NC}"
    echo ""
    read -p "Press ENTER after fixing permissions, or Ctrl+C to cancel..."
fi

# Step 1: Build the documentation
echo -e "\n${BLUE}[1/3]${NC} Building documentation with Emacs..."
docker compose up one-el --build

if [ $? -ne 0 ]; then
    echo -e "${RED}✗ Build failed!${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Build completed${NC}"

# Step 2: Verify build output
echo -e "\n${BLUE}[2/3]${NC} Verifying build output..."
if [ ! -d "$LOCAL_BUILD_PATH" ]; then
    echo -e "${RED}✗ Build directory not found: $LOCAL_BUILD_PATH${NC}"
    exit 1
fi

FILE_COUNT=$(find "$LOCAL_BUILD_PATH" -type f | wc -l)
echo -e "${GREEN}✓ Found $FILE_COUNT files to deploy${NC}"

# Step 3: Deploy to server
echo -e "\n${BLUE}[3/3]${NC} Deploying to $REMOTE_HOST..."

# Create a temporary directory on remote
TEMP_DIR="/tmp/django-liveview-docs-deploy-$$"
ssh "${REMOTE_USER}@${REMOTE_HOST}" "mkdir -p ${TEMP_DIR}"

# Copy files to temp directory
echo -e "${BLUE}Copying files...${NC}"
scp -rq "${LOCAL_BUILD_PATH}"* "${REMOTE_USER}@${REMOTE_HOST}:${TEMP_DIR}/"

# Move files to final destination
echo -e "${BLUE}Installing files...${NC}"
ssh "${REMOTE_USER}@${REMOTE_HOST}" "bash -c 'rm -rf ${REMOTE_PATH}/* && mv ${TEMP_DIR}/* ${REMOTE_PATH}/ && rmdir ${TEMP_DIR}'"

if [ $? -ne 0 ]; then
    echo -e "${RED}✗ Deploy failed!${NC}"
    exit 1
fi

echo -e "\n${GREEN}=========================================${NC}"
echo -e "${GREEN}✓ Deployment completed successfully!${NC}"
echo -e "${GREEN}=========================================${NC}"
echo -e "\nDocs are now live at: ${BLUE}https://docs.djangoliveview.dev${NC}"
