# version 150

uniform vec3 triangleColor;
in vec2 textureCoord;

out vec4 outColor;
uniform sampler2D ourTexture;

void main()
{
        outColor = texture(ourTexture, textureCoord);
}
