MUTABLE_INTERP="interpreter.lisp"
MUTABLE_TEST="test_script.lisp"

FUNC_INTERP="functional_interpreter.lisp"
FUNC_TEST="test_functional.lisp"

if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL no está instalado."
    exit 1
fi

echo "=========================================="
echo "mutable interpreter test"
echo "file: $MUTABLE_INTERP"
echo "=========================================="

if [ -f "$MUTABLE_INTERP" ] && [ -f "$MUTABLE_TEST" ]; then
    sbcl --script "$MUTABLE_INTERP" < "$MUTABLE_TEST" 2>/dev/null | grep "=>"
else
    echo "error: missing files (mutable)."
fi
echo "" 

echo "=========================================="
echo "functional interpreter (Lambda Calculus)"
echo "file: $FUNC_INTERP"
echo "=========================================="

if [ -f "$FUNC_INTERP" ] && [ -f "$FUNC_TEST" ]; then
    sbcl --script "$FUNC_INTERP" < "$FUNC_TEST" 2>/dev/null | grep "=>"
else
    echo "ERROR: missing files (funcional)."
fi

echo ""
echo "=========================================="
echo "testing completed."
echo "=========================================="