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
    # TODO: Add any additional filters or globals if needed

    return j2_env
