import { Resolver, Query, Mutation, Arg, Ctx, ID } from 'type-graphql'
import { Review, ReviewCreateInput, ReviewUpdateInput, ReviewQueryArgs, ReviewConnection } from '../../schema'
import { Context } from './types'

@Resolver(() => Review)
export class ReviewResolver {
	@Query(() => Review, { nullable: true })
	async review(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<Review | null> {
		const result = await ctx.prisma.review.findUnique({
			where: { id },
			include: {
				product: true,
			},
		})

		return result as Review | null
	}

	@Query(() => ReviewConnection)
	async reviews(@Arg('args', () => ReviewQueryArgs, { nullable: true }) args: ReviewQueryArgs, @Ctx() ctx: Context): Promise<ReviewConnection> {
		const take = args?.first || 10
		const skip = args?.after ? 1 : 0
		const cursor = args?.after ? { id: args.after } : undefined

		const reviews = await ctx.prisma.review.findMany({
			take: take + 1,
			skip,
			cursor,
			where: this.buildWhereCondition(args?.filter),
			orderBy: this.buildOrderBy(args?.sort),
			include: {
				product: true,
			},
		})

		const hasNextPage = reviews.length > take
		const nodes = hasNextPage ? reviews.slice(0, -1) : reviews

		const edges = nodes.map((review) => ({
			node: review as Review,
			cursor: review.id,
		}))

		return {
			edges,
			pageInfo: {
				hasNextPage,
				hasPreviousPage: false,
				startCursor: edges[0]?.cursor,
				endCursor: edges[edges.length - 1]?.cursor,
			},
			totalCount: await ctx.prisma.review.count({
				where: this.buildWhereCondition(args?.filter),
			}),
		}
	}

	@Mutation(() => Review)
	async createReview(@Arg('input', () => ReviewCreateInput) input: ReviewCreateInput, @Ctx() ctx: Context): Promise<Review> {
		const result = await ctx.prisma.review.create({
			data: input,
			include: {
				product: true,
			},
		})

		return result as Review
	}

	@Mutation(() => Review)
	async updateReview(@Arg('id', () => ID) id: string, @Arg('input', () => ReviewUpdateInput) input: ReviewUpdateInput, @Ctx() ctx: Context): Promise<Review> {
		const result = await ctx.prisma.review.update({
			where: { id },
			data: input,
			include: {
				product: true,
			},
		})

		return result as Review
	}

	@Mutation(() => Boolean)
	async deleteReview(@Arg('id', () => ID) id: string, @Ctx() ctx: Context): Promise<boolean> {
		await ctx.prisma.review.delete({
			where: { id },
		})
		return true
	}

	private buildWhereCondition(filter: any) {
		if (!filter) return undefined

		const where: any = {}

		if (filter.title) {
			where.title = { contains: filter.title.contains || filter.title.equals }
		}

		if (filter.rating) {
			where.rating = filter.rating.equals || filter.rating
		}

		if (filter.verified !== undefined) {
			where.verified = filter.verified.equals !== undefined ? filter.verified.equals : filter.verified
		}

		if (filter.createdAt) {
			where.createdAt = {}
			if (filter.createdAt.gte) where.createdAt.gte = filter.createdAt.gte
			if (filter.createdAt.lte) where.createdAt.lte = filter.createdAt.lte
		}

		// Handle AND/OR operations
		if (filter.AND) {
			where.AND = filter.AND.map((f: any) => this.buildWhereCondition(f))
		}

		if (filter.OR) {
			where.OR = filter.OR.map((f: any) => this.buildWhereCondition(f))
		}

		return where
	}

	private buildOrderBy(sort: any) {
		if (!sort) return { createdAt: 'desc' }

		const orderBy: any = {}

		if (sort.title) orderBy.title = sort.title.toLowerCase()
		if (sort.rating) orderBy.rating = sort.rating.toLowerCase()
		if (sort.createdAt) orderBy.createdAt = sort.createdAt.toLowerCase()
		if (sort.helpfulCount) orderBy.helpfulCount = sort.helpfulCount.toLowerCase()

		return orderBy
	}
}
