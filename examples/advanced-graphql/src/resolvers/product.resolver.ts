import { Resolver, Query, Mutation, Arg, Ctx, ID, Info, Int, FieldResolver, Root } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Product, ProductCreateInput, ProductUpdateInput, ProductConnection, ProductFilterInput, ProductSortInput, ProductTag, Review } from '../../schema'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import type { Context } from './types'
import { PRODUCT_WITH_ALL_RELATIONS_SELECT, PRODUCT_TAG_WITH_RELATIONS_SELECT, REVIEW_WITH_PRODUCT_SELECT } from '../select-definitions'

@Resolver(() => Product)
export class ProductResolver {
	@Query(() => Product, { nullable: true })
	async product(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Product | null> {
		return (await ctx.prisma.product.findUnique({
			where: { id },
			select: PRODUCT_WITH_ALL_RELATIONS_SELECT,
		})) as Product | null
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
		const where = buildFilter(filter as any)
		const orderBy = buildSort(sort as any)
		const select = buildSelect(PRODUCT_WITH_ALL_RELATIONS_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		const [items, totalCount] = await Promise.all([ctx.prisma.product.findMany(config.findMany), ctx.prisma.product.count(config.count)])

		return config.toConnection(items, totalCount) as ProductConnection
	}

	@Mutation(() => Product)
	async createProduct(@Arg('input', () => ProductCreateInput) input: ProductCreateInput, @Ctx() ctx: Context): Promise<Product> {
		return (await ctx.prisma.product.create({
			data: input,
			select: PRODUCT_WITH_ALL_RELATIONS_SELECT,
		})) as Product
	}

	@Mutation(() => Product)
	async updateProduct(
		@Arg('id', () => ID) id: string,
		@Arg('input', () => ProductUpdateInput) input: ProductUpdateInput,
		@Ctx() ctx: Context,
	): Promise<Product> {
		return (await ctx.prisma.product.update({
			where: { id },
			data: input,
			select: PRODUCT_WITH_ALL_RELATIONS_SELECT,
		})) as Product
	}

	@Mutation(() => Boolean)
	async deleteProduct(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.product.delete({ where: { id } })
		return true
	}

	@FieldResolver(() => [ProductTag])
	async tags(@Root() product: Product, @Ctx() ctx: Context): Promise<ProductTag[]> {
		return (await ctx.prisma.productTag.findMany({
			where: { productId: product.id },
			select: PRODUCT_TAG_WITH_RELATIONS_SELECT,
		})) as ProductTag[]
	}

	@FieldResolver(() => [Review])
	async reviews(@Root() product: Product, @Ctx() ctx: Context): Promise<Review[]> {
		return (await ctx.prisma.review.findMany({
			where: { productId: product.id },
			select: REVIEW_WITH_PRODUCT_SELECT,
		})) as Review[]
	}
}
