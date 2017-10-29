# version 150

in vec2 position;
in vec2 texCoord;
in vec3 inColor;

uniform mat4 transform;

out vec2 textureCoord;
out vec3 color;

void main()
{
        gl_Position = transform * vec4(position, 0.0, 1.0);
        textureCoord = texCoord;
        color = inColor;
}
