import { Resolver, Query, FieldResolver, Root, Ctx, Arg, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { PostCategory, Post, Category, PostCategoryConnection } from '../../schema'
import type { Context } from './types'
import { buildConnection, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import { POST_CATEGORY_WITH_RELATIONS_SELECT, POST_WITH_ALL_RELATIONS_SELECT, CATEGORY_WITH_POSTS_SELECT } from '../select-definitions'

@Resolver(() => PostCategory)
export class PostCategoryResolver {
	@Query(() => PostCategoryConnection)
	async postCategories(
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<PostCategoryConnection> {
		const select = buildSelect(POST_CATEGORY_WITH_RELATIONS_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			select,
		})

		const [items, totalCount] = await Promise.all([prisma.categoryOnPost.findMany(config.findMany), prisma.categoryOnPost.count(config.count)])

		return config.toConnection(items, totalCount) as PostCategoryConnection
	}

	@FieldResolver(() => Post)
	async post(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Post | null> {
		return (await prisma.post.findUnique({ where: { id: postCategory.postId }, select: POST_WITH_ALL_RELATIONS_SELECT })) as Post | null
	}

	@FieldResolver(() => Category)
	async category(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Category | null> {
		return (await prisma.category.findUnique({ where: { id: postCategory.categoryId }, select: CATEGORY_WITH_POSTS_SELECT })) as Category | null
	}
}
