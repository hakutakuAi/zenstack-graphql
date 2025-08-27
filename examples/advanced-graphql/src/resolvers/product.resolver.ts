import { Resolver, Query, Mutation, Arg, Ctx, ID, Info, Int, FieldResolver, Root } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import {
	Product,
	ProductCreateInput,
	ProductUpdateInput,
	ProductQueryArgs,
	ProductConnection,
	ProductFilterInput,
	ProductSortInput,
	ProductTag,
	Review,
} from '../../schema'
import { ConnectionBuilder } from '../../schema-helpers'
import type { Context } from './types'

@Resolver(() => Product)
export class ProductResolver {
	@Query(() => Product, { nullable: true })
	async product(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Product | null> {
		return (await ctx.prisma.product.findUnique({ where: { id } })) as Product | null
	}

	@Query(() => ProductConnection)
	async products(
		@Arg('filter', () => ProductFilterInput, { nullable: true }) filter: ProductFilterInput | undefined,
		@Arg('sort', () => ProductSortInput, { nullable: true }) sort: ProductSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() ctx: Context,
	): Promise<ProductConnection> {
		const args: ProductQueryArgs = { filter, sort, first, after, last, before }
		const config = ConnectionBuilder.buildProductConnectionConfig(args, info)

		const items = await ctx.prisma.product.findMany(config.findManyOptions)
		const totalCount = await ctx.prisma.product.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as ProductConnection
	}

	@Mutation(() => Product)
	async createProduct(@Arg('input', () => ProductCreateInput) input: ProductCreateInput, @Ctx() ctx: Context): Promise<Product> {
		return (await ctx.prisma.product.create({ data: input })) as Product
	}

	@Mutation(() => Product)
	async updateProduct(
		@Arg('id', () => ID) id: string,
		@Arg('input', () => ProductUpdateInput) input: ProductUpdateInput,
		@Ctx() ctx: Context,
	): Promise<Product> {
		return (await ctx.prisma.product.update({ where: { id }, data: input })) as Product
	}

	@Mutation(() => Boolean)
	async deleteProduct(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.product.delete({ where: { id } })
		return true
	}

	@FieldResolver(() => [ProductTag])
	async tags(@Root() product: Product, @Ctx() ctx: Context): Promise<ProductTag[]> {
		return (await ctx.prisma.productTag.findMany({ where: { productId: product.id } })) as ProductTag[]
	}

	@FieldResolver(() => [Review])
	async reviews(@Root() product: Product, @Ctx() ctx: Context): Promise<Review[]> {
		return (await ctx.prisma.review.findMany({ where: { productId: product.id } })) as Review[]
	}
}
