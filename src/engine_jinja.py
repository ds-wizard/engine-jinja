import json

import jinja2
import jinja2.sandbox


def render_jinja(input_data: str) -> str:
    # Load the input data
    try:
        data = json.loads(input_data)
        contexts = data.get('contexts', [])
        template_str = data.get('template', None)
    except Exception as e:
        return _out_error(
            message=f'Invalid JSON input: {e}.',
        )
    if contexts == [] or contexts is None:
        return _out_results([])
    n_contexts = len(contexts)
    if template_str is None:
        return _out_error(
            message='Input JSON data must contain "template" key.',
            repeat=n_contexts,
        )

    # Prepare the Jinja environment
    try:
        j2_env = _prepare_jinja_env()
    except Exception as e:
        return _out_error(
            message=f'Error preparing Jinja environment: {e}.',
            repeat=n_contexts,
        )

    # Prepare the Jinja template
    try:
        j2_template = j2_env.from_string(template_str)
    except Exception as e:
        return _out_error(
            message=f'Error preparing Jinja template: {e}.',
            repeat=n_contexts,
        )

    # Render template with input data
    results = []
    for context in contexts:
        try:
            result = j2_template.render(**context)
            results.append(_success(result=result))
        except jinja2.TemplateError as e:
            results.append(_error(message=f'Template rendering error: {e}'))
        except Exception as e:
            results.append(_error(message=f'An unexpected error occurred: {e}'))

    # Dump results to JSON
    return _out_results(results)


def _error(message) -> dict:
    return {
        'result': '',
        'ok': False,
        'message': message,
    }


def _success(result, message=None) -> dict:
    return {
        'result': result,
        'ok': True,
        'message': message,
    }


def _out_error(message: str, repeat: int = 1) -> str:
    results = []
    for _ in range(repeat):
        results.append(_error(message=message))
    return json.dumps(results)


def _out_results(results: list) -> str:
    return json.dumps(results)


def _prepare_jinja_env():
    j2_env = jinja2.sandbox.SandboxedEnvironment(
        loader=jinja2.BaseLoader(),
        autoescape=jinja2.select_autoescape(['html', 'xml']),
        extensions=[
            'jinja2.ext.do',
            'jinja2.ext.loopcontrols',
        ]
    )
    _add_filters(j2_env)
    return j2_env

# ====================================================================================
# Custom filters
# ====================================================================================
def _filter_endswith(value, suffix):
    """Check if the value ends with the specified suffix."""
    return str(value).endswith(suffix)

def _filter_startswith(value, prefix):
    """Check if the value starts with the specified prefix."""
    return str(value).startswith(prefix)

def _index_of(value, item):
    """Get the index of the first occurrence of item in value."""
    if isinstance(value, list):
        try:
            return value.index(item)
        except ValueError:
            return -1
    return -1

def _filter_intercalate(value, separator):
    """Join a list of strings with the specified separator."""
    if isinstance(value, list):
        return separator.join(str(v) for v in value)
    return str(value)

def _filter_split(value, separator):
    """Split a string by the specified separator."""
    if isinstance(value, str):
        return value.split(separator)
    return []

def _filter_of_alphabet(value):
    """Convert a number to its corresponding letter in the alphabet (1 -> 'a', 2 -> 'b', ..., 28 -> 'aa')."""
    if not isinstance(value, int) or value < 1:
        return ''
    alphabet = 'abcdefghijklmnopqrstuvwxyz'
    result = ''
    while value > 0:
        value -= 1
        result = alphabet[value % 26] + result
        value //= 26
    return result

def _filter_roman(value):
    """Convert an integer to a Roman numeral."""
    if not isinstance(value, int) or value <= 0:
        return ''
    val = [
        1000, 900, 500, 400,
        100, 90, 50, 40,
        10, 9, 5, 4,
        1
    ]
    syms = [
        "M", "CM", "D", "CD",
        "C", "XC", "L", "XL",
        "X", "IX", "V", "IV",
        "I"
    ]
    roman_numeral = ''
    for i in range(len(val)):
        while value >= val[i]:
            roman_numeral += syms[i]
            value -= val[i]
    return roman_numeral

def _filter_any(value):
    """Check if any element in the value is truthy."""
    return any(value)

def _filter_all(value):
    """Check if all elements in the value are truthy."""
    return all(value)

def _add_filters(j2_env):
    j2_env.filters['endswith'] = _filter_endswith
    j2_env.filters['startswith'] = _filter_startswith
    j2_env.filters['indexOf'] = _index_of
    j2_env.filters['intercalate'] = _filter_intercalate
    j2_env.filters['split'] = _filter_split
    j2_env.filters['ofAlphabet'] = _filter_of_alphabet
    j2_env.filters['roman'] = _filter_roman
    j2_env.filters['any'] = _filter_any
    j2_env.filters['all'] = _filter_all
