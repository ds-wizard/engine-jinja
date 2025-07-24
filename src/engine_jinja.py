import json

import jinja2
import jinja2.sandbox


def render_jinja(input_data: str) -> str:
    try:
        return _render_jinja(input_data)
    except Exception as e:
        return f'{"ok": false, "error": "{e.__class__.__name__}"}'


def _render_jinja(input_data: str) -> str:
    try:
        data = json.loads(input_data)
    except json.JSONDecodeError as e:
        return json.dumps({
            'error': f'Invalid JSON input: {e}',
            'ok': False
        })

    # Check if data is a dictionary
    if not isinstance(data, dict):
        return json.dumps({
            'error': 'Input data must be a JSON object (dictionary).',
            'ok': False
        })

    input_template = data.get('template', None)
    input_context = data.get('context', None)
    if input_template is None or input_context is None:
        return json.dumps({
            'error': 'Input JSON data must contain "template" and "context" keys.',
            'ok': False
        })
    template_str = input_template or ''
    context = input_context or {}

    # Create a Jinja2 environment (use sandbox for security)
    env = jinja2.sandbox.SandboxedEnvironment(
        loader=jinja2.BaseLoader(),
        autoescape=jinja2.select_autoescape(['html', 'xml']),
        extensions=[
            'jinja2.ext.do',
            'jinja2.ext.loopcontrols',
        ]
    )
    # TODO: Add any additional filters or globals if needed

    try:
        # Create a template from the string
        jinja_template = env.from_string(template_str)

        # Render template with input data
        result = jinja_template.render(**context)

        # Done, exit gracefully
        return json.dumps({
            'result': result,
            'ok': True
        })
    except jinja2.TemplateError as e:
        return json.dumps({
            'error': f'Template rendering error: {e}',
            'ok': False
        })
    except Exception as e:
        return json.dumps({
            'error': f'Unexpected error: {e}',
            'ok': False
        })
