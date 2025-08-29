import { Resolver, Query, Mutation, Arg, Ctx, ID, Info, Int, FieldResolver, Root } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Review, ReviewCreateInput, ReviewUpdateInput, ReviewConnection, ReviewFilterInput, ReviewSortInput, Product } from '../../schema'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import type { Context } from './types'
import { REVIEW_WITH_PRODUCT_SELECT, PRODUCT_WITH_ALL_RELATIONS_SELECT } from '../select-definitions'

@Resolver(() => Review)
export class ReviewResolver {
	@Query(() => Review, { nullable: true })
	async review(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Review | null> {
		return (await ctx.prisma.review.findUnique({
			where: { id },
			select: REVIEW_WITH_PRODUCT_SELECT,
		})) as Review | null
	}

	@Query(() => ReviewConnection)
	async reviews(
		@Arg('filter', () => ReviewFilterInput, { nullable: true }) filter: ReviewFilterInput | undefined,
		@Arg('sort', () => ReviewSortInput, { nullable: true }) sort: ReviewSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() ctx: Context,
	): Promise<ReviewConnection> {
		const where = buildFilter(filter as any)
		const orderBy = buildSort(sort as any)
		const select = buildSelect(REVIEW_WITH_PRODUCT_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		const [items, totalCount] = await Promise.all([ctx.prisma.review.findMany(config.findMany), ctx.prisma.review.count(config.count)])

		return config.toConnection(items, totalCount) as ReviewConnection
	}

	@Mutation(() => Review)
	async createReview(@Arg('input', () => ReviewCreateInput) input: ReviewCreateInput, @Ctx() ctx: Context): Promise<Review> {
		return (await ctx.prisma.review.create({
			data: input,
			select: REVIEW_WITH_PRODUCT_SELECT,
		})) as Review
	}

	@Mutation(() => Review)
	async updateReview(@Arg('id', () => ID) id: string, @Arg('input', () => ReviewUpdateInput) input: ReviewUpdateInput, @Ctx() ctx: Context): Promise<Review> {
		return (await ctx.prisma.review.update({
			where: { id },
			data: input,
			select: REVIEW_WITH_PRODUCT_SELECT,
		})) as Review
	}

	@Mutation(() => Boolean)
	async deleteReview(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.review.delete({ where: { id } })
		return true
	}

	@FieldResolver(() => Product, { nullable: true })
	async product(@Root() review: Review, @Ctx() ctx: Context): Promise<Product | null> {
		return (await ctx.prisma.product.findUnique({
			where: { id: review.productId },
			select: PRODUCT_WITH_ALL_RELATIONS_SELECT,
		})) as Product | null
	}
}
