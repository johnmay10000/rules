# Gemini Setup Guide

This guide explains how to use the Gemini guides in your own projects.

## Using the Guides

There are a few ways to use the Gemini guides in your project:

1.  **Using a Script:** You can use a script to fetch the latest version of the guidelines from the repository and copy them into your project. This is a flexible approach that can be customized to your needs.

    Here is an example of a simple shell script to do this:

    ```bash
    #!/bin/bash

    # The URL of the repository where the guidelines are stored
    REPO_URL="<repository-url>"

    # The directory where the guidelines will be stored in your project
    DEST_DIR=".gemini"

    # Create the destination directory if it doesn't exist
    mkdir -p "$DEST_DIR"

    # Fetch the latest version of the guidelines and copy them to the destination directory
    git archive --remote="$REPO_URL" --format=tar HEAD:gemini | tar -x -C "$DEST_DIR"

    echo "Gemini guidelines updated successfully."
    ```

2.  **Copying the Files:** This is a simpler approach for new projects. You can simply copy the `gemini` directory into your project's root and rename it to `.gemini`.

3. **Git Subtree:** For more advanced use cases, you can use `git subtree`. This method embeds the guidelines' history within your project's history.

    ```bash
    git subtree add --prefix .gemini <repository-url> gemini --squash
    ```
