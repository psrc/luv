# LUV RUNNER
# ----------------------------------------------------
URBANSIM_URL=https://github.com/psrc/urbansim.git
URBANSIM_VERSION=master

DB_GOLDEN_URL=
DB_STAGING_URL=

JURISDICTIONAL_CONTROL_TOTALS= # todo: how do we import these totals? [filename?]


# ----------------------------------------------------
# Install all dependencies: git, python, etc
pip install -r requirements.txt

# ----------------------------------------------------
# Fetch urbansim
git clone $URBANSIM_URL urbansim
cd urbansim; git checkout $URBANSIM_VERSION ; cd ..

# ----------------------------------------------------
# Fetch golden standard database

# ----------------------------------------------------
# Stage all database changes

# ----------------------------------------------------
# Store snapshot somewhere

# ----------------------------------------------------
# Write out cache

# ----------------------------------------------------
# Check for sanity

# ----------------------------------------------------
# Run urbansim!

# ----------------------------------------------------
# Summarize everything

# ----------------------------------------------------
# Store summaries somewhere
