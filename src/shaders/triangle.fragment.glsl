# version 150

in vec2 textureCoord;

out vec4 outColor;
uniform sampler2D ourTexture;

void main()
{
        outColor = texture(ourTexture, textureCoord);
}
