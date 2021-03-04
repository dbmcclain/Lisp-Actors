// ECCTest.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "../ecc.h"

int main(int argc, char* argv[])
{
	printf("Start...\n");
	fflush(stdout);

	gf_opnd k("2C662948FF9A216FE4EDB5421D2068F9B9AB37FE550D9C713BABA0CB02835AE2ECD27E1D9F6FC7F94858550DBB4D3E3B394EC49FF77F65F518EDA40BFB1AC6E15BDFB24079E84D4");
	k.show();
	printf("\n");

#if 0
	k.two();
	k.show();
	printf("\n");
#endif

	gf_opnd tmp1;
	gf_opnd prod;

#if 0
	prod.one();
	for(int ix = 0; ix < 580; ++ix)
	{
		gf_mul(k, prod, prod);
		// prod.show();
		// printf("\n");

		gf_mul(k, k, k);
		k.show();
		printf("\n");
	}
#endif
	gf_inv(k, tmp1);
	tmp1.show();
	printf("\n");

	gf_mul(k, tmp1, prod);
	prod.show();
	printf("\n");

	gf_div(k, k, prod);
	prod.show();
	printf("\n");

	const char* gstr[2] = {"303001d34b856296c16c0d40d3cd7750a93d1d2955fa80aa5f40fc8db7b2abdbde53950f4c0d293cdd711a35b67fb1499ae60038614f1394abfa3b4c850d927e1e7769c8eec2d19",
		"37bf27342da639b6dccfffeb73d69d78c6c27a6009cbbca1980f8533921e8a684423e43bab08a576291af8f461bb2a8b3531d2f0485c19b16e2f1516e23dd3c1a4827af1b8ac15b"};

	printf("G: ");
	ecc_affine_pt g(gstr);
	g.show();

	printf("-G: ");
	ecc_affine_pt g2;
	ecc_neg(g, g2);
	g2.show();

	printf("G-G: ");
	ecc_sub(g,g,g2);
	g2.show();

	printf("G+G: ");
	ecc_add(g,g,g2);
	g2.show();

	printf("2*G: ");
	k.two();
	ecc_mul(g, k, g2);
	g2.show();

	printf("2*G-G: ");
	ecc_sub(g2,g,g2);
	g2.show();

	gf_opnd ihash("58A860D56D85011CD75FD9F6FB0C7558C6E2AD04CBCA7CC5BB0A34CB2749DB20");
	gf_opnd phash("F059872EBD036FC3E5F93A82AD416E9A57DA21A1DE40D913AD7E94705A3B1E82");
	gf_opnd fhash("5356793B25AEDD8613962AED2481625722D8C9F9F4345E5F7FCF969F6DC835DC");
	const char* ckeyStr[2] = {
		"0A42E3FB18B05752951613CEDEE91D88737F738DFE90052DE79DDE0D4663D647C1115BBE90F3D8C423468053D9FB883E74CA3E351232F7D2CDDEF95E80C10293E4F8CE428F8EC57",
		"6CCC2AA031A0F99BBAB493CB29D7FCADA50397460022DC7E444FAA4248C1CF23D68CDFC3AB4ADE052D65661156294F06F9A25AA7BA5340A8B9659633A9BD12EEFAACF390332DB69"
	};
	ecc_affine_pt ckey(ckeyStr);

	ecc_affine_pt g3;

	printf("Hi:");
	ihash.show();
	printf("\n");

	printf("Hp:");
	phash.show();
	printf("\n");

	printf("Hf:");
	fhash.show();
	printf("\n");

	gf_mul(ihash, phash, k);
	gf_mul(fhash, k, k);
	printf("Hi*Hp*Hf:");
	k.show();
	printf("\n");

	ecc_mulGen(k, g2);
	printf("Hi*Hp*Hf*G: ");
	g2.show();

	ecc_add(g2, ckey, g3);
	printf("Ecc Decrypt: ");
	g3.show();

#if 0
	ecc_mulGen(ihash, g2);
	printf("Hi*G: ");
	g2.show();

	ecc_mulGen(phash, g3);
	printf("Hp*G: ");
	g3.show();

	ecc_add(g2, g3, g2);
	ecc_add(ckey, g2, g2);
	printf("Ecc Decrypt: ");
	g2.show();
#endif

	fflush(stdout);
	// getchar();
	return 0;
}

