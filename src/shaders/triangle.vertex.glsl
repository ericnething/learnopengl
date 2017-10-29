# version 150

in vec3 position;
in vec2 texCoord;
in vec3 inColor;

uniform mat4 transform;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec2 textureCoord;
out vec3 color;

void main()
{
        gl_Position = projection * view * model * transform * vec4(position, 1.0);
        textureCoord = texCoord;
        color = inColor;
}
