import { Resolver, FieldResolver, Root, Ctx } from 'type-graphql'
import { ProductTag, Product, Tag } from '../../schema'
import type { Context } from './types'

@Resolver(() => ProductTag)
export class ProductTagResolver {
	@FieldResolver(() => Product)
	async product(@Root() productTag: ProductTag, @Ctx() ctx: Context): Promise<Product> {
		return (await ctx.prisma.product.findUnique({ where: { id: productTag.productId } })) as Product
	}

	@FieldResolver(() => Tag)
	async tag(@Root() productTag: ProductTag, @Ctx() ctx: Context): Promise<Tag> {
		return (await ctx.prisma.tag.findUnique({ where: { id: productTag.tagId } })) as Tag
	}
}
