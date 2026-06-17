#!/bin/bash
# Helper script to analyze benchmark output from scip-R

set -e

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <benchmark_log_file> [command]"
    echo ""
    echo "Commands:"
    echo "  summary    - Show overall summary (default)"
    echo "  slowest    - Show top 10 slowest operations"
    echo "  counts     - Show call counts"
    echo "  by-op      - Group by operation type and sum durations"
    echo "  by-file    - Show per-file timings"
    echo "  phases     - Show phase breakdown"
    echo "  all        - Show all analyses"
    exit 1
fi

LOG_FILE="$1"
COMMAND="${2:-summary}"

if [ ! -f "$LOG_FILE" ]; then
    echo "Error: File not found: $LOG_FILE"
    exit 1
fi

# Extract just the JSON part from benchmark lines
extract_json() {
    grep '^\[BENCH\]' "$LOG_FILE" | sed 's/^\[BENCH\] //'
}

# Summary
show_summary() {
    echo "=== Benchmark Summary ==="
    echo ""

    total_time=$(extract_json | jq -s 'map(select(.op == "index_project_total")) | .[0].duration_ms // 0')
    file_count=$(extract_json | jq -s 'map(select(.op == "index_project_total")) | .[0].file_count // 0')

    echo "Total indexing time: ${total_time}ms"
    echo "Files processed: ${file_count}"

    if [ "$file_count" != "0" ]; then
        avg=$(echo "scale=2; $total_time / $file_count" | bc)
        echo "Average per file: ${avg}ms"
    fi
    echo ""
}

# Top slowest operations
show_slowest() {
    echo "=== Top 10 Slowest Operations ==="
    echo ""
    extract_json | jq -s 'map(select(.duration_ms != null)) | sort_by(.duration_ms) | reverse | .[0:10] | .[] | "\(.op): \(.duration_ms)ms \(if .file then "(\(.file))" else "" end)"' -r
    echo ""
}

# Call counts
show_counts() {
    echo "=== Call Counts ==="
    echo ""
    extract_json | jq -s 'map(select(.call_count != null)) | sort_by(.call_count) | reverse | .[] | "\(.op): \(.call_count) calls"' -r
    echo ""
}

# Group by operation type
show_by_op() {
    echo "=== Time by Operation Type ==="
    echo ""
    extract_json | jq -s 'group_by(.op) | map({op: .[0].op, total_ms: (map(.duration_ms // 0) | add), count: length, avg_ms: (map(.duration_ms // 0) | add / length)}) | sort_by(.total_ms) | reverse | .[] | "\(.op): \(.total_ms)ms (count: \(.count), avg: \(.avg_ms)ms)"' -r
    echo ""
}

# Per-file timings
show_by_file() {
    echo "=== Per-File Timings ==="
    echo ""
    echo "Parse times:"
    extract_json | jq -s 'map(select(.op == "parse_file" and .file != null)) | sort_by(.duration_ms) | reverse | .[] | "  \(.file): \(.duration_ms)ms"' -r
    echo ""
    echo "Processing times:"
    extract_json | jq -s 'map(select(.op == "process_file_with_table" and .file != null)) | sort_by(.duration_ms) | reverse | .[] | "  \(.file): \(.duration_ms)ms"' -r
    echo ""
}

# Phase breakdown
show_phases() {
    echo "=== Phase Breakdown ==="
    echo ""
    extract_json | jq -s 'map(select(.op | startswith("phase") or startswith("mirai_phase"))) | .[] | "\(.op): \(.duration_ms)ms"' -r
    echo ""
}

# Execute requested command
case "$COMMAND" in
    summary)
        show_summary
        ;;
    slowest)
        show_slowest
        ;;
    counts)
        show_counts
        ;;
    by-op)
        show_by_op
        ;;
    by-file)
        show_by_file
        ;;
    phases)
        show_phases
        ;;
    all)
        show_summary
        show_slowest
        show_counts
        show_by_op
        show_by_file
        show_phases
        ;;
    *)
        echo "Unknown command: $COMMAND"
        exit 1
        ;;
esac
